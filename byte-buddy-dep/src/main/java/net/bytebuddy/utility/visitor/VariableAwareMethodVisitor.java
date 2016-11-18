package net.bytebuddy.utility.visitor;

import net.bytebuddy.description.method.MethodDescription;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class VariableAwareMethodVisitor extends MethodVisitor {

    private int offset;

    public VariableAwareMethodVisitor(MethodVisitor methodVisitor, MethodDescription instrumentedMethod) {
        super(Opcodes.ASM5, methodVisitor);
        offset = instrumentedMethod.getStackSize();
    }

    @Override
    public void visitVarInsn(int opcode, int offset) {
        super.visitVarInsn(opcode, offset);
        this.offset = Math.max(this.offset, offset);
    }

    public int register(int size) {
        try {
            return offset;
        } finally {
            offset += size;
        }
    }

    @Override
    public void visitMaxs(int operandStackSize, int localVariableLength) {
        super.visitMaxs(operandStackSize, Math.max(localVariableLength, offset));
    }
}
