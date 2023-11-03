package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.reporter.ReporterModel;
import org.assertj.core.api.Condition;
import org.assertj.core.api.SoftAssertions;
import org.assertj.core.api.WithAssertions;
import org.assertj.core.api.WithAssumptions;
import org.assertj.core.api.junit.jupiter.SoftAssertionsExtension;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationApplication;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.NetworkModificationException.Type;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.core.type.filter.AssignableTypeFilter;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

@DisplayNameGeneration(DisplayNameGenerator.Simple.class)
@ExtendWith(SoftAssertionsExtension.class)
@Tag("UnitTest")
class ModificationInfosTest implements WithAssertions, WithAssumptions {
    @Test
    void testGetTypeNullWhenNoAnnotation() {
        final ModificationInfos modification = new ModificationInfos() { };
        assertThat(modification.getType()).as("modification type").isNull();
    }

    @Test
    void testGetTypeValueWhenAnnotation() {
        final ModificationInfos modification = new ModificationWithAnnotations();
        assertThat(modification.getType()).as("modification type").isEqualTo(ModificationType.VSC_CREATION);
    }

    @Test
    void testGetErrorTypeNullWhenNoAnnotation() {
        final ModificationInfos modification = new ModificationInfos() { };
        assertThat(modification.getErrorType()).as("modification error type").isNull();
    }

    @Test
    void testGetErrorTypeValueWhenAnnotation() {
        final ModificationInfos modification = new ModificationWithAnnotations();
        assertThat(modification.getErrorType()).as("modification error type").isEqualTo(Type.CREATE_VSC_ERROR);
    }

    @ModificationErrorTypeName("CREATE_VSC_ERROR")
    @JsonTypeName("VSC_CREATION")
    private static class ModificationWithAnnotations extends ModificationInfos {
        // ...
    }

    private static List<Class<? extends ModificationInfos>> allImplementations = new ArrayList<>();
    private static Set<Class<? extends ModificationInfos>> intermediatesImplementations;
    private static Set<Class<? extends ModificationInfos>> terminalImplementations;

    @BeforeAll
    static void searchClasses() throws ClassNotFoundException {
        { //we search all children classes of ModificationInfos
            final ClassPathScanningCandidateComponentProvider scanner = new ClassPathScanningCandidateComponentProvider(false);
            scanner.addIncludeFilter(new AssignableTypeFilter(ModificationInfos.class)); //we search all children classes
            for (final BeanDefinition beanDef : scanner.findCandidateComponents(NetworkModificationApplication.class.getPackageName())) {
                final String clazzName = beanDef.getBeanClassName();
                if (ModificationInfos.class.getCanonicalName().equals(clazzName) ||
                        ModificationWithAnnotations.class.getName().equals(clazzName)) {
                    continue; //skip parent, we want only children and no test class
                }
                allImplementations.add((Class<? extends ModificationInfos>) Class.forName(clazzName));
            }
        }
        intermediatesImplementations = allImplementations.stream()
                .map(Class::getSuperclass)
                //.filter(Predicate.not(ModificationInfos.class::equals))
                .map(c -> (Class<? extends ModificationInfos>) c) //some generics' reification problem
                .collect(Collectors.toUnmodifiableSet());
        terminalImplementations = allImplementations.stream()
                .filter(Predicate.not(intermediatesImplementations::contains))
                .collect(Collectors.toUnmodifiableSet());
    }

    private static List<Arguments> provideIntermediateImplementationsWithClasses()
            throws NoSuchMethodException, InvocationTargetException, InstantiationException, IllegalAccessException {
        final List<Arguments> arguments = new ArrayList<>(intermediatesImplementations.size());
        for (final Class<? extends ModificationInfos> clazz : intermediatesImplementations) {
            final ModificationInfos instance = clazz.getDeclaredConstructor().newInstance(); //use default (empty) constructor
            arguments.add(Arguments.of(clazz.getSimpleName(), clazz, instance));
        }
        return arguments;
    }

    private static List<Arguments> provideTerminalImplementationsWithClasses() {
        final List<Arguments> arguments = new ArrayList<>(terminalImplementations.size());
        for (final Class<?> clazz : terminalImplementations) {
            arguments.add(Arguments.of(clazz.getSimpleName(), clazz));
        }
        return arguments;
    }

    private static List<Arguments> provideTerminalImplementationsWithInstances()
            throws NoSuchMethodException, InvocationTargetException, InstantiationException, IllegalAccessException {
        final List<Arguments> arguments = new ArrayList<>(terminalImplementations.size());
        for (final Class<? extends ModificationInfos> clazz : terminalImplementations) {
            final ModificationInfos instance = clazz.getDeclaredConstructor().newInstance(); //use default (empty) constructor
            arguments.add(Arguments.of(clazz.getSimpleName(), instance));
        }
        return arguments;
    }

    @ParameterizedTest(name = "for class {0}")
    @MethodSource("provideIntermediateImplementationsWithClasses")
    void checkIntermediateClassesDoesntOverrideOrAnnotate(final String name, final Class<? extends ModificationInfos> clazz,
                                                          final ModificationInfos instance, final SoftAssertions softly) {
        softly.assertThat(clazz).satisfies(
            _class -> assertThat(_class.isAnnotationPresent(JsonTypeName.class)).as("@JsonTypeName present").isFalse(),
            _class -> assertThat(_class.isAnnotationPresent(ModificationErrorTypeName.class)).as("@ModificationErrorTypeName present").isFalse()
        );
        //softly.assertThat(catchThrowable(instance::toEntity)).isInstanceOf(UnsupportedOperationException.class);
        //softly.assertThat(catchThrowable(instance::toModification)).isInstanceOf(UnsupportedOperationException.class);
        /*softly.assertThat(catchThrowable(() -> instance.createSubReporter(new ReporterModel("test", "test reporter"))))
                .isInstanceOf(UnsupportedOperationException.class);*/
    }

    @ParameterizedTest(name = "for class {0}")
    @MethodSource("provideTerminalImplementationsWithInstances")
    void checkMethodsUnsupportedOperationOverrides(final String name, final ModificationInfos instance, final SoftAssertions softly) throws UnsupportedOperationException {
        try {
            softly.assertThat(instance.toEntity()).isNotNull();
        } catch (final NullPointerException rEx) {
            //Assumptions.abort("conversion append on uninitialized fields");
        } catch (final NetworkModificationException ex) {
            //Assumptions.abort("internal check on uninitialized field prevent verification");
        }
        if (!(instance instanceof ConverterStationCreationInfos)) { //exception because CSCI is included in VscCreationInfos (HVCD case)
            softly.assertThat(instance.toModification()).isNotNull();
            try {
                softly.assertThat(instance.createSubReporter(new ReporterModel("test", "test reporter"))).isNotNull();
            } catch (final NullPointerException rEx) {
                //Assumptions.abort("sub reporter template need uninitialized value");
            }
        }
    }

    @Test
    void isAllSubTypesDeclared() throws Exception {
        assertThat(ModificationInfos.class.getAnnotation(JsonSubTypes.class).value())
                .as("ModificationInfos Jackson SubTypes declared")
                .isNotEmpty()
                .extracting(JsonSubTypes.Type::value)
                .doesNotHaveDuplicates()
                .containsExactlyInAnyOrder(terminalImplementations.toArray(Class[]::new)); //use an array to bypass generic validation
    }

    /*
     * because else error in ModificationInfos.getType()
     */
    @ParameterizedTest(name = "{0}")
    @MethodSource("provideTerminalImplementationsWithClasses")
    void isAllJsonNamesValid(final String name, final Class<? extends ModificationInfos> clazz) throws IllegalArgumentException {
        final Condition<String> isInModificationType = new Condition<>(s -> ModificationType.valueOf(s) != null, "is a ModificationType value");
        assertThat(clazz.getAnnotation(JsonTypeName.class).value()).as("implementations class Json name type")
                .satisfies(isInModificationType);
    }

    /*
     * because else error in ModificationInfos.getErrorType()
     */
    @ParameterizedTest(name = "{0}")
    @MethodSource("provideTerminalImplementationsWithClasses")
    void isAllErrorTypesValid(final String name, final Class<? extends ModificationInfos> clazz) throws IllegalArgumentException {
        final Condition<String> isInModificationType = new Condition<>(s -> Type.valueOf(s) != null, "is a NetworkModificationException Type value");
        assertThat(clazz.getAnnotation(ModificationErrorTypeName.class).value()).as("implementations class Json name type")
                .satisfies(isInModificationType);
    }
}
