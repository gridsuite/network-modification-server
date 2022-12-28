package org.gridsuite.modification.server.modifications;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.google.common.io.ByteStreams;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.SneakyThrows;
import okhttp3.mockwebserver.Dispatcher;
import okhttp3.mockwebserver.MockResponse;
import okhttp3.mockwebserver.MockWebServer;
import okhttp3.mockwebserver.RecordedRequest;
import okio.Buffer;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationApplication;
import org.gridsuite.modification.server.VariationMode;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.FilterInfo;
import org.gridsuite.modification.server.dto.GeneratorScalingInfos;
import org.gridsuite.modification.server.dto.GeneratorScalingVariation;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.utils.MatcherGeneratorScalingInfos;
import org.gridsuite.modification.server.utils.MatcherModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.SendInput;
import org.jetbrains.annotations.NotNull;
import org.junit.After;
import org.junit.Before;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.cloud.stream.binder.test.InputDestination;
import org.springframework.cloud.stream.binder.test.TestChannelBinderConfiguration;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Objects;
import java.util.UUID;

import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig;
import static org.gridsuite.modification.server.utils.NetworkUtil.createGenerator;
import static org.junit.Assert.assertEquals;

@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
@SpringBootTest
@ContextConfiguration(classes = {NetworkModificationApplication.class, TestChannelBinderConfiguration.class})
public class GeneratorScalingModificationTest extends AbstractNetworkModificationTest{

    private static final UUID GENERATOR_SCALING_ID = UUID.randomUUID();
    private static final String FILTER_ID_1 = "bdefd63f-6cd8-4686-b57b-6bc7aaffa202";
    private static final String FILTER_ID_2 = "bdfad63f-6fe6-4686-b57b-6bc7aa11a202";
    private static final String FILTER_ID_3 = "bd063f-611f-4686-b57b-6bc7aa00a202";
    private static final String FILTER_ID_4 = "6f11d63f-6f06-4686-b57b-6bc7aa66a202";
    private static final String FILTER_ID_5 = "7100163f-60f1-4686-b57b-6bc7aa77a202";
    private static final String GENERATOR_ID_1 = "idGenerator";
    private static final String GENERATOR_ID_2 = "v5generator";
    private static final String GENERATOR_ID_3 = "v6generator";
    private static final String GENERATOR_ID_4 = "gen4";
    private static final String GENERATOR_ID_5 = "gen5";
    private static final String GENERATOR_ID_6 = "gen6";
    private static final String GENERATOR_ID_7 = "gen7";
    private static final String GENERATOR_ID_8 = "gen8";
    private static final String GENERATOR_ID_9 = "gen9";
    private static final String GENERATOR_ID_10 = "gen10";

    // new mock server (use this one to mock API calls)
    private WireMockServer wireMock;

    private InputDestination input;

    private MockWebServer server;

    @Autowired
    private FilterService filterService;

    @Before
    public void specificSetUp() throws IOException {
        server = new MockWebServer();
        createGenerator(getNetwork().getVoltageLevel("v1"), GENERATOR_ID_4, 3, 100, 1.0, "cn10", 11, ConnectablePosition.Direction.TOP, 500, -1);
        createGenerator(getNetwork().getVoltageLevel("v1"), GENERATOR_ID_5, 20, 200, 1.0, "cn10", 12, ConnectablePosition.Direction.TOP, 2000, -1);
        createGenerator(getNetwork().getVoltageLevel("v2"), GENERATOR_ID_6, 11, 100, 1.0, "cn10", 13, ConnectablePosition.Direction.TOP, 500, -1);
        createGenerator(getNetwork().getVoltageLevel("v6"), GENERATOR_ID_7, 10, 200, 1.0, "cn10", 14, ConnectablePosition.Direction.TOP, 2000, -1);
        createGenerator(getNetwork().getVoltageLevel("v3"), GENERATOR_ID_8, 10, 100, 1.0, "cn10", 15, ConnectablePosition.Direction.TOP, 500, -1);
        createGenerator(getNetwork().getVoltageLevel("v4"), GENERATOR_ID_9, 10, 200, 1.0, "cn10", 16, ConnectablePosition.Direction.TOP, 2000, -1);
        createGenerator(getNetwork().getVoltageLevel("v5"), GENERATOR_ID_10, 10, 100, 1.0, "cn10", 17, ConnectablePosition.Direction.TOP, 500, -1);

        var test = "[{\"id\":\"bdefd63f-6cd8-4686-b57b-6bc7aaffa202\",\"modificationDate\":\"2022-12-12T15:00:26.911+00:00\",\"equipmentType\":\"GENERATOR\",\"filterEquipmentsAttributes\":[{\"equipmentID\":\"idGenerator\",\"distributionKey\":1},{\"equipmentID\":\"gen5\",\"distributionKey\":2}],\"type\":\"IDENTIFIER_LIST\"},{\"id\":\"bdfad63f-6fe6-4686-b57b-6bc7aa11a202\",\"modificationDate\":\"2022-12-12T15:00:27.911+00:00\",\"equipmentType\":\"GENERATOR\",\"filterEquipmentsAttributes\":[{\"equipmentID\":\"gen4\"},{\"equipmentID\":\"gen7\"}],\"type\":\"IDENTIFIER_LIST\"},{\"id\":\"bd063f-611f-4686-b57b-6bc7aa00a202\",\"modificationDate\":\"2022-12-12T15:00:28.911+00:00\",\"equipmentType\":\"GENERATOR\",\"filterEquipmentsAttributes\":[{\"equipmentID\":\"gen6\",\"distributionKey\":1},{\"equipmentID\":\"v6generator\",\"distributionKey\":0}],\"type\":\"IDENTIFIER_LIST\"},{\"id\":\"6f11d63f-6f06-4686-b57b-6bc7aa66a202\",\"modificationDate\":\"2022-12-12T15:00:26.911+00:00\",\"equipmentType\":\"GENERATOR\",\"filterEquipmentsAttributes\":[{\"equipmentID\":\"gen8\",\"distributionKey\":1},{\"equipmentID\":\"v5generator\",\"distributionKey\":2}],\"type\":\"IDENTIFIER_LIST\"},{\"id\":\"7100163f-60f1-4686-b57b-6bc7aa77a202\",\"modificationDate\":\"2022-12-12T15:00:26.911+00:00\",\"equipmentType\":\"GENERATOR\",\"filterEquipmentsAttributes\":[{\"equipmentID\":\"gen9\",\"distributionKey\":1},{\"equipmentID\":\"gen10\",\"distributionKey\":2}],\"type\":\"IDENTIFIER_LIST\"}]";
        var jsonBody = resourceToString("/Filter_equipments.json");
        server.start();

        var uri = "http://" + server.getHostName() + ":" + server.getPort();
        filterService.setFilterServerBaseUri(uri);
        final Dispatcher dispatcher = new Dispatcher() {

            @SneakyThrows
            @Override
            public MockResponse dispatch(RecordedRequest recordedRequest) {
                String path = Objects.requireNonNull(recordedRequest.getPath());
                Buffer body = recordedRequest.getBody();

                String params = String.join(",", List.of(FILTER_ID_1, FILTER_ID_2, FILTER_ID_3, FILTER_ID_4, FILTER_ID_5));
                if (path.matches("/v1/filters/data\\?ids=" + params) && "GET".equals(recordedRequest.getMethod())) {
                    return new MockResponse()
                            .setResponseCode(200)
                            .addHeader("Content-Type", "application/json; charset=utf-8")
                            .setBody(test);
                }
                return new MockResponse().setResponseCode(418).setBody(path);
            }
        };
        /*input = new InputDestination();
        wireMock = new WireMockServer(wireMockConfig().dynamicPort().extensions(new SendInput(input)));
        wireMock.start();
        String params = String.join(",", List.of(FILTER_ID_1, FILTER_ID_2, FILTER_ID_3, FILTER_ID_4, FILTER_ID_5));
        var test = wireMock.stubFor(WireMock.get(WireMock.urlPathEqualTo("/v1/filters/data?ids=" + params))
                .willReturn(WireMock.ok()
                        .withBody(resourceToString("/Filter_equipments.json"))
                        .withHeader("Content-Type", "application/json")));*/
        server.setDispatcher(dispatcher);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        var filter1 = FilterInfo.builder()
                .id(FILTER_ID_1)
                .name("filter 1")
                .build();

        var filter2 = FilterInfo.builder()
                .id(FILTER_ID_2)
                .name("filter 2")
                .build();

        var filter3 = FilterInfo.builder()
                .id(FILTER_ID_3)
                .name("filter 3")
                .build();

        var filter4 = FilterInfo.builder()
                .id(FILTER_ID_4)
                .name("filter 3")
                .build();

        var filter5 = FilterInfo.builder()
                .id(FILTER_ID_5)
                .name("filter 3")
                .build();

        var variation1 = GeneratorScalingVariation.builder()
                .variationMode(VariationMode.PROPORTIONAL_TO_PMAX)
                .variationValue(50D)
                .filters(List.of(filter1))
                .build();

        var variation2 = GeneratorScalingVariation.builder()
                .variationMode(VariationMode.REGULAR_DISTRIBUTION)
                .variationValue(50D)
                .filters(List.of(filter2))
                .build();

        var variation3 = GeneratorScalingVariation.builder()
                .variationMode(VariationMode.STACKING_UP)
                .variationValue(50D)
                .filters(List.of(filter3))
                .build();

        var variation4 = GeneratorScalingVariation.builder()
                .variationMode(VariationMode.VENTILATION)
                .variationValue(50D)
                .filters(List.of(filter4))
                .build();

        var variation5 = GeneratorScalingVariation.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .variationValue(50D)
                .filters(List.of(filter1, filter5))
                .build();

        return GeneratorScalingInfos.builder()
                .uuid(GENERATOR_SCALING_ID)
                .date(ZonedDateTime.now())
                .type(ModificationType.GENERATOR_SCALING)
                .isIterative(true)
                .variationType(VariationType.DELTA_P)
                .generatorScalingVariations(List.of(variation1, variation2, variation3, variation4, variation5))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        var filter5 = FilterInfo.builder()
                .id(FILTER_ID_5)
                .name("filter 3")
                .build();

        var variation5 = GeneratorScalingVariation.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .variationValue(50D)
                .filters(List.of(filter5))
                .build();

        return GeneratorScalingInfos.builder()
                .uuid(GENERATOR_SCALING_ID)
                .date(ZonedDateTime.now())
                .type(ModificationType.GENERATOR_SCALING)
                .isIterative(false)
                .variationType(VariationType.DELTA_P)
                .generatorScalingVariations(List.of(variation5))
                .build();
    }

    @Override
    protected MatcherModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherGeneratorScalingInfos.createMatcherGeneratorScalingInfos((GeneratorScalingInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_1).getTargetP(), 68.82, 0.01D);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_2).getTargetP(), 75.43, 0.01D);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_3).getTargetP(), 42.1, 0.01D);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_4).getTargetP(), 125.0, 0.01D);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_5).getTargetP(), 273.27, 0.01D);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_6).getTargetP(), 150, 0.01D);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_7).getTargetP(), 225, 0.01D);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_8).getTargetP(), 116.66, 0.01D);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_9).getTargetP(), 233.33, 0.01D);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_10).getTargetP(), 116.66, 0.01D);
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_1).getTargetP(), 42.1, 0);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_2).getTargetP(), 42.1, 0);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_3).getTargetP(), 42.1, 0);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_4).getTargetP(), 100, 0);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_5).getTargetP(), 200, 0);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_6).getTargetP(), 100, 0);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_7).getTargetP(), 200, 0);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_8).getTargetP(), 100, 0);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_9).getTargetP(), 200, 0);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_10).getTargetP(), 100, 0);
    }

    private String resourceToString(String resource) throws IOException {
        return new String(ByteStreams.toByteArray(getClass().getResourceAsStream(resource)), StandardCharsets.UTF_8);
    }

    @After
    public void shutDown() throws IOException {
        //wireMock.shutdown();
        server.shutdown();
    }
}
