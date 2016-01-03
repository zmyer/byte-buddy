package net.bytebuddy.dynamic.scaffold;

import net.bytebuddy.description.field.FieldDescription;
import net.bytebuddy.description.type.TypeDescription;
import net.bytebuddy.implementation.attribute.AnnotationAppender;
import net.bytebuddy.implementation.attribute.FieldAttributeAppender;
import net.bytebuddy.matcher.ElementMatcher;
import net.bytebuddy.matcher.LatentMatcher;

import java.util.*;

/**
 * A field registry represents an extendable collection of fields which are identified by their names that are mapped
 * to a given {@link net.bytebuddy.implementation.attribute.FieldAttributeAppender}. Fields
 * can be uniquely identified by their name for a given type since fields are never inherited.
 * <p>&nbsp;</p>
 * This registry is the counterpart of a {@link net.bytebuddy.dynamic.scaffold.MethodRegistry}.
 * However, a field registry is implemented simpler since it does not have to deal with complex signatures or
 * inheritance. For the sake of consistency, the field registry follows however a similar pattern without introducing
 * unnecessary complexity.
 */
public interface FieldRegistry {

    FieldRegistry include(LatentMatcher<? super FieldDescription> matcher, FieldAttributeAppender.Factory attributeAppenderFactory, Object defaultValue);

    /**
     * Prepares the field registry for a given instrumented type.
     *
     * @param instrumentedType The instrumented type.
     * @return A prepared field registry.
     */
    Compiled compile(TypeDescription instrumentedType);

    /**
     * Represents a compiled field registry.
     */
    interface Compiled extends TypeWriter.FieldPool {

        /**
         * A no-op field registry that does not register annotations for any field.
         */
        enum NoOp implements Compiled {

            INSTANCE;

            @Override
            public Record target(FieldDescription fieldDescription) {
                return new Record.ForSimpleField(fieldDescription, AnnotationAppender.ValueFilter.Default.APPEND_DEFAULTS); // TODO
            }

            @Override
            public String toString() {
                return "FieldRegistry.Compiled.NoOp." + name();
            }
        }
    }

    /**
     * An immutable default implementation of a field registry.
     */
    class Default implements FieldRegistry {

        private final List<Entry> entries;

        public Default() {
            entries = Collections.emptyList();
        }

        private Default(List<Entry> entries) {
            this.entries = entries;
        }

        @Override
        public FieldRegistry include(LatentMatcher<? super FieldDescription> matcher, FieldAttributeAppender.Factory attributeAppenderFactory, Object defaultValue) {
            List<Entry> entries = new ArrayList<Entry>(this.entries.size() + 1);
            entries.add(new Entry(matcher, attributeAppenderFactory, defaultValue));
            entries.addAll(this.entries);
            return new Default(entries);
        }

        @Override
        public FieldRegistry.Compiled compile(TypeDescription instrumentedType) {
            List<Compiled.Entry> entries = new ArrayList<Compiled.Entry>(this.entries.size());
            Map<FieldAttributeAppender.Factory, FieldAttributeAppender> attributeAppenders = new HashMap<FieldAttributeAppender.Factory, FieldAttributeAppender>();
            for (Entry entry : this.entries) {
                FieldAttributeAppender attributeAppender = attributeAppenders.get(entry.getAttributeAppenderFactory());
                if (attributeAppender == null) {
                    attributeAppender = entry.getAttributeAppenderFactory().make(instrumentedType);
                    attributeAppenders.put(entry.getAttributeAppenderFactory(), attributeAppender);
                }
                entries.add(new Compiled.Entry(entry.resolve(instrumentedType), attributeAppender, entry.getDefaultValue()));
            }
            return new Compiled(entries);
        }

        @Override
        public boolean equals(Object other) {
            return this == other || !(other == null || getClass() != other.getClass())
                    && entries.equals(((Default) other).entries);
        }

        @Override
        public int hashCode() {
            return entries.hashCode();
        }

        @Override
        public String toString() {
            return "FieldRegistry.Default{entries=" + entries + '}';
        }

        /**
         * An entry of the default field registry.
         */
        protected static class Entry implements LatentMatcher<FieldDescription> {

            private final LatentMatcher<? super FieldDescription> matcher;

            /**
             * The field attribute appender factory that is represented by this entry.
             */
            private final FieldAttributeAppender.Factory attributeAppenderFactory;

            /**
             * The field's default value for this entry.
             */
            private final Object defaultValue;

            /**
             * Creates a new entry.
             *
             * @param attributeAppenderFactory The field attribute appender factory that is represented by this entry.
             * @param defaultValue             The field's default value for this entry.
             */
            protected Entry(LatentMatcher<? super FieldDescription> matcher, FieldAttributeAppender.Factory attributeAppenderFactory, Object defaultValue) {
                this.matcher = matcher;
                this.attributeAppenderFactory = attributeAppenderFactory;
                this.defaultValue = defaultValue;
            }

            /**
             * Returns the field attribute appender factory.
             *
             * @return The field's attribute appender factory.
             */
            protected FieldAttributeAppender.Factory getAttributeAppenderFactory() {
                return attributeAppenderFactory;
            }

            /**
             * Returns the default value.
             *
             * @return The default value.
             */
            protected Object getDefaultValue() {
                return defaultValue;
            }

            @Override
            public ElementMatcher<? super FieldDescription> resolve(TypeDescription instrumentedType) {
                return matcher.resolve(instrumentedType);
            }

            @Override
            public boolean equals(Object other) {
                if (this == other) return true;
                if (other == null || getClass() != other.getClass()) return false;
                Entry entry = (Entry) other;
                return matcher.equals(entry.matcher)
                        && attributeAppenderFactory.equals(entry.attributeAppenderFactory)
                        && !(defaultValue != null ? !defaultValue.equals(entry.defaultValue) : entry.defaultValue != null);
            }

            @Override
            public int hashCode() {
                int result = matcher.hashCode();
                result = 31 * result + attributeAppenderFactory.hashCode();
                result = 31 * result + (defaultValue != null ? defaultValue.hashCode() : 0);
                return result;
            }

            @Override
            public String toString() {
                return "FieldRegistry.Default.Entry{" +
                        "matcher=" + matcher +
                        ", attributeAppenderFactory=" + attributeAppenderFactory +
                        ", defaultValue=" + defaultValue +
                        '}';
            }
        }

        /**
         * A compiled default field registry.
         */
        protected static class Compiled implements FieldRegistry.Compiled {

            private final List<Entry> entries;

            protected Compiled(List<Entry> entries) {
                this.entries = entries;
            }

            @Override
            public Record target(FieldDescription fieldDescription) {
                for (Entry entry : entries) {
                    if (entry.matches(fieldDescription)) {
                        return entry.bind(fieldDescription);
                    }
                }
                return new Record.ForSimpleField(fieldDescription, AnnotationAppender.ValueFilter.Default.APPEND_DEFAULTS); // TODO
            }

            @Override
            public boolean equals(Object other) {
                return this == other || !(other == null || getClass() != other.getClass())
                        && entries.equals(((Compiled) other).entries);
            }

            @Override
            public int hashCode() {
                return 31 * entries.hashCode();
            }

            @Override
            public String toString() {
                return "FieldRegistry.Default.Compiled{" +
                        "entries=" + entries +
                        '}';
            }

            /**
             * An entry of a compiled field registry.
             */
            protected static class Entry implements ElementMatcher<FieldDescription> {

                private final ElementMatcher<? super FieldDescription> matcher;

                /**
                 * The attribute appender to be applied to any bound field.
                 */
                private final FieldAttributeAppender attributeAppender;

                /**
                 * The default value to be set for any bound field or {@code null} if no default value should be set.
                 */
                private final Object defaultValue;

                /**
                 * Creates a new entry.
                 *
                 * @param attributeAppender The attribute appender to be applied to any bound field.
                 * @param defaultValue      The default value to be set for any bound field or {@code null} if no default value should be set.
                 */
                protected Entry(ElementMatcher<? super FieldDescription> matcher, FieldAttributeAppender attributeAppender, Object defaultValue) {
                    this.matcher = matcher;
                    this.attributeAppender = attributeAppender;
                    this.defaultValue = defaultValue;
                }

                /**
                 * Binds this entry to the provided field description.
                 *
                 * @param fieldDescription The field description to be bound to this entry.
                 * @return A record representing the binding of this entry to the provided field.
                 */
                protected Record bind(FieldDescription fieldDescription) {
                    return new Record.ForRichField(attributeAppender, defaultValue, fieldDescription);
                }

                @Override
                public boolean matches(FieldDescription target) {
                    return matcher.matches(target);
                }

                @Override
                public boolean equals(Object other) {
                    if (this == other) return true;
                    if (other == null || getClass() != other.getClass()) return false;
                    Entry entry = (Entry) other;
                    return matcher.equals(entry.matcher)
                            && attributeAppender.equals(entry.attributeAppender)
                            && !(defaultValue != null ? !defaultValue.equals(entry.defaultValue) : entry.defaultValue != null);
                }

                @Override
                public int hashCode() {
                    int result = matcher.hashCode();
                    result = 31 * attributeAppender.hashCode();
                    result = 31 * result + (defaultValue != null ? defaultValue.hashCode() : 0);
                    return result;
                }

                @Override
                public String toString() {
                    return "FieldRegistry.Default.Compiled.Entry{" +
                            "matcher=" + matcher +
                            ", attributeAppender=" + attributeAppender +
                            ", defaultValue=" + defaultValue +
                            '}';
                }
            }
        }
    }
}
