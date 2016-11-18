package net.bytebuddy.asm;

import net.bytebuddy.description.field.FieldDescription;
import net.bytebuddy.description.field.FieldList;
import net.bytebuddy.description.method.MethodDescription;
import net.bytebuddy.description.method.MethodList;
import net.bytebuddy.description.type.TypeDescription;
import net.bytebuddy.dynamic.scaffold.MethodGraph;
import net.bytebuddy.implementation.Implementation;
import net.bytebuddy.implementation.bytecode.StackManipulation;
import net.bytebuddy.implementation.bytecode.member.FieldAccess;
import net.bytebuddy.matcher.ElementMatcher;
import net.bytebuddy.pool.TypePool;
import net.bytebuddy.utility.visitor.VariableAwareMethodVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import java.util.ArrayList;
import java.util.List;

import static net.bytebuddy.matcher.ElementMatchers.*;

public class Substitution implements AsmVisitorWrapper.ForDeclaredMethods.MethodVisitorWrapper {

    private final TypePoolProvider typePoolProvider;

    private final Transformer transformer;

    private final ElementMatcher<? super TypeDescription> ignoreMatcher;

    protected Substitution(TypePoolProvider typePoolProvider, Transformer transformer, ElementMatcher<? super TypeDescription> ignoreMatcher) {
        this.typePoolProvider = typePoolProvider;
        this.transformer = transformer;
        this.ignoreMatcher = ignoreMatcher;
    }

    @Override
    public MethodVisitor wrap(TypeDescription instrumentedType,
                              MethodDescription.InDefinedShape instrumentedMethod,
                              MethodVisitor methodVisitor,
                              Implementation.Context implementationContext,
                              TypePool typePool,
                              int writerFlags,
                              int readerFlags) {
        return new SubstitutingMethodVisitor(methodVisitor,
                instrumentedMethod,
                typePoolProvider.resolve(typePool),
                transformer,
                ignoreMatcher,
                implementationContext);
    }

    public Substitution fieldRead(ElementMatcher<? super FieldDescription.InDefinedShape> matcher) {
        return null;
    }

    public Substitution fieldWrite(ElementMatcher<? super FieldDescription.InDefinedShape> matcher) {
        return null;
    }

    public Substitution method(ElementMatcher<? super MethodDescription.InDefinedShape> matcher) {
        return null;
    }

    public static class Matched extends Substitution {

        private final List<MethodDescription.InDefinedShape> substitutions = new ArrayList<MethodDescription.InDefinedShape>();

        protected Matched(TypePoolProvider typePoolProvider, Transformer transformer, ElementMatcher<? super TypeDescription> ignoreMatcher) {
            super(typePoolProvider, transformer, ignoreMatcher);
        }
    }

    protected interface TypePoolProvider {

        TypePool resolve(TypePool typePool);

        enum Implicit implements TypePoolProvider {

            INSTANCE;

            @Override
            public TypePool resolve(TypePool typePool) {
                return typePool;
            }
        }

        class Explicit implements TypePoolProvider {

            private final TypePool typePool;

            protected Explicit(TypePool typePool) {
                this.typePool = typePool;
            }

            @Override
            public TypePool resolve(TypePool typePool) {
                return this.typePool;
            }
        }
    }

    protected interface Substitute {

        StackManipulation ofRead(FieldDescription.InDefinedShape fieldDescription);

        StackManipulation ofWrite(FieldDescription.InDefinedShape fieldDescription);

        StackManipulation of(MethodDescription.InDefinedShape methodDescription);

        class ForField implements Substitute {

            private final FieldDescription.InDefinedShape fieldDescription;

            protected ForField(FieldDescription.InDefinedShape fieldDescription) {
                this.fieldDescription = fieldDescription;
            }

            @Override
            public StackManipulation ofRead(FieldDescription.InDefinedShape fieldDescription) {
                if (!this.fieldDescription.getType().asErasure().isAssignableTo(fieldDescription.getType().asErasure())) {
                    throw new IllegalStateException();
                }
                return FieldAccess.forField(this.fieldDescription).read();
            }

            @Override
            public StackManipulation ofWrite(FieldDescription.InDefinedShape fieldDescription) {
                if (!fieldDescription.getType().asErasure().isAssignableTo(this.fieldDescription.getType().asErasure())) {
                    throw new IllegalStateException();
                }
                return FieldAccess.forField(this.fieldDescription).write();
            }

            @Override
            public StackManipulation of(MethodDescription.InDefinedShape methodDescription) {
                if (methodDescription.getParameters().isEmpty() && fieldDescription.getType().asErasure().isAssignableTo(methodDescription.getReturnType().asErasure())) {
                    return FieldAccess.forField(this.fieldDescription).read();
                } else if (methodDescription.getParameters().size() == 1
                        && methodDescription.getParameters().get(0).getType().asErasure().isAssignableTo(fieldDescription.getType().asErasure())
                        && methodDescription.getReturnType().represents(void.class)) {
                    return FieldAccess.forField(this.fieldDescription).write();
                } else {
                    throw new IllegalStateException();
                }
            }
        }
    }

    protected interface Transformer {

        StackManipulation resolve(FieldDescription.InDefinedShape fieldDescription, boolean write);

        StackManipulation resolve(MethodDescription.InDefinedShape methodDescription, TypeDescription owner, boolean special);

        enum NoOp implements Transformer {

            INSTANCE;

            @Override
            public StackManipulation resolve(FieldDescription.InDefinedShape fieldDescription, boolean write) {
                return StackManipulation.Illegal.INSTANCE;
            }

            @Override
            public StackManipulation resolve(MethodDescription.InDefinedShape methodDescription, TypeDescription owner, boolean special) {
                return StackManipulation.Illegal.INSTANCE;
            }
        }

        class Active implements Transformer {

            private final List<? extends Substitute> substitutes;

            public Active(List<? extends Substitute> substitutes) {
                this.substitutes = substitutes;
            }

            @Override
            public StackManipulation resolve(FieldDescription.InDefinedShape fieldDescription, boolean write) {
                List<StackManipulation> stackManipulations = new ArrayList<StackManipulation>(substitutes.size());
                for (Substitute substitute : substitutes) {
                    stackManipulations.add(write
                            ? substitute.ofWrite(fieldDescription)
                            : substitute.ofRead(fieldDescription));
                }
                return new StackManipulation.Compound(stackManipulations);
            }

            @Override
            public StackManipulation resolve(MethodDescription.InDefinedShape methodDescription, TypeDescription owner, boolean special) {
                List<StackManipulation> stackManipulations = new ArrayList<StackManipulation>(substitutes.size());
                for (Substitute substitute : substitutes) {
                    stackManipulations.add(substitute.of(methodDescription));
                }
                return new StackManipulation.Compound(stackManipulations);
            }
        }
    }

    protected static class SubstitutingMethodVisitor extends VariableAwareMethodVisitor {

        private final TypePool typePool;

        private final Transformer transformer;

        private final ElementMatcher<? super TypeDescription> ignoreMatcher;

        private final Implementation.Context implementationContext;


        protected SubstitutingMethodVisitor(MethodVisitor methodVisitor,
                                            MethodDescription instrumentedMethod,
                                            TypePool typePool,
                                            Transformer transformer,
                                            ElementMatcher<? super TypeDescription> ignoreMatcher,
                                            Implementation.Context implementationContext) {
            super(methodVisitor, instrumentedMethod);
            this.typePool = typePool;
            this.transformer = transformer;
            this.ignoreMatcher = ignoreMatcher;
            this.implementationContext = implementationContext;
        }

        @Override
        public void visitFieldInsn(int opcode, String owner, String name, String descriptor) {
            TypePool.Resolution resolution = typePool.describe(owner.replace('/', '.')); // Method might be virtual!
            if (resolution.isResolved() && !ignoreMatcher.matches(resolution.resolve())) {
                FieldList<FieldDescription.InDefinedShape> candidates = resolution.resolve()
                        .getDeclaredFields()
                        .filter(named(name).and(hasDescriptor(descriptor)));
                if (candidates.size() == 1) {
                    StackManipulation stackManipulation = transformer.resolve(candidates.getOnly(), opcode == Opcodes.PUTFIELD || opcode == Opcodes.PUTSTATIC);
                    if (stackManipulation.isValid()) {
                        stackManipulation.apply(mv, implementationContext);
                        return;
                    }
                }
            }
            super.visitFieldInsn(opcode, owner, name, descriptor);
        }

        @Override
        public void visitMethodInsn(int opcode, String owner, String name, String descriptor, boolean isInterface) {
            if (!name.equals(MethodDescription.TYPE_INITIALIZER_INTERNAL_NAME)) {
                TypePool.Resolution resolution = typePool.describe(owner.replace('/', '.'));
                if (resolution.isResolved() && !ignoreMatcher.matches(resolution.resolve())) {
                    MethodList<MethodDescription.InDefinedShape> candidates;
                    if (opcode == Opcodes.INVOKESTATIC) {
                        candidates = resolution.resolve().getDeclaredMethods().filter(named(name).and(hasDescriptor(descriptor)));
                    } else if (name.equals(MethodDescription.CONSTRUCTOR_INTERNAL_NAME)) {
                        candidates = resolution.resolve().getDeclaredMethods().filter(isConstructor().and(hasDescriptor(descriptor)));
                    } else {
                        candidates = resolution.resolve().getDeclaredMethods().filter(named(name).and(hasDescriptor(descriptor)));
                        if (candidates.isEmpty()) {
                            candidates = MethodGraph.Compiler.DEFAULT.compile(resolution.resolve())
                                    .listNodes()
                                    .asMethodList()
                                    .asDefined()
                                    .filter(named(name).and(hasDescriptor(descriptor)));
                        }
                    }
                    if (candidates.size() == 1) {
                        StackManipulation stackManipulation = transformer.resolve(candidates.getOnly(), resolution.resolve(), opcode == Opcodes.INVOKESPECIAL);
                        if (stackManipulation.isValid()) {
                            stackManipulation.apply(mv, implementationContext);
                            return;
                        }
                    }
                }
            }
            super.visitMethodInsn(opcode, owner, name, descriptor, isInterface);
        }
    }
}
