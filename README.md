# Network Modification Server

[![Actions Status](https://github.com/gridsuite/network-modification-server/actions/workflows/build.yml/badge.svg?branch=main)](https://github.com/gridsuite/network-modification-server/actions)
[![Coverage Status](https://sonarcloud.io/api/project_badges/measure?project=org.gridsuite%3Anetwork-modification-server&metric=coverage)](https://sonarcloud.io/component_measures?id=org.gridsuite%3Anetwork-modification-server&metric=coverage)
[![MPL-2.0 License](https://img.shields.io/badge/license-MPL_2.0-blue.svg)](https://www.mozilla.org/en-US/MPL/2.0/)

## Description

The **network-modification-server** is a microservice of the [GridSuite](https://github.com/gridsuite) platform dedicated to **managing and applying network modifications**.

It provides the following capabilities:

- **Store and manage network modifications** organised in **groups**: a group is an ordered container of modifications tied to a study variant. It is the unit applied during a network build. Supports create, read, update, delete, move, copy, stash/unstash.
- **Apply modifications** to network variants in the network store, either immediately (incremental mode) or as part of a full variant build.
- **Support composite modifications**: a **composite** is a named, reusable modification that encapsulates a sequence of individual modifications **or other composites** (recursively). Unlike a group (which is bound to a variant), a composite is an independent entity that can be inserted into one or more groups (in `INSERT` or `SPLIT` mode), duplicated, and reorganised.
- **Build network variants asynchronously** via RabbitMQ: apply an ordered sequence of modification groups onto a cloned variant, with support for cancellation.
- **Index modification impacts** in Elasticsearch: track which equipment IDs were created, modified, or deleted by each modification, and expose a full-text search API.
- **Expose a line types catalog**: predefined line electrical parameters (area, temperature, shape factor, limits) to assist modification forms.
- **Resolve equipment filters** via the filter-server for modifications that reference filter-based equipment selection (e.g. generation dispatch).

---


## Technical Stack

- Spring Boot (Web, Data JPA, Actuator)
- PostgreSQL + Liquibase
- RabbitMQ via Spring Cloud Stream
- Elasticsearch (`spring-data-elasticsearch`)
- PowSyBl network store client (`powsybl-network-store-client`)
- [Network modification library](https://github.com/gridsuite/network-modification)
- API documentation: OpenAPI / Swagger (`springdoc`)
- Micrometer / Prometheus

---


## Architecture: library vs server

The [`network-modification`](https://github.com/gridsuite/network-modification) library is a **framework-agnostic Java library** that defines the DTOs and the logic to apply each modification type onto a PowSyBl `Network`. It has no Spring or HTTP dependency and can be used in any JVM project.

The **network-modification-server** builds on top of this library to expose its capabilities as a REST microservice: it handles persistence (PostgreSQL), async variant builds (RabbitMQ), Elasticsearch indexation, and integration with the rest of the GridSuite platform.

---


## Development Scripts

Build Docker image

```shell
mvn install -DskipTests -Dpowsybl.docker.install
```

Please read [liquibase usage](https://github.com/powsybl/powsybl-parent/#liquibase-usage) for instructions to automatically generate changesets. After you generated a changeset do not forget to add it to git and in `src/main/resources/db/changelog/db.changelog-master.yaml`.

---

## Interactions with Other Microservices

```text
┌────────────────────────────────┐
│  network-modification-server   │──► network-store-server  (read/write network variants)
│                                │──► filter-server          (resolve equipment filters)
│                                │──► report-server          (post computation logs)
└────────────────────────────────┘
             ▲  ▼
          RabbitMQ (build.run / build.cancel / build.result / build.stopped)
```

---

## Asynchronous Build Flow

1. The caller sends `POST /v1/networks/{networkUuid}/build` — the server publishes a message on the `build.run` queue.
2. Parallel consumers (`consumeBuild1`, `consumeBuild2`) pick up build messages for load balancing.
3. For each modification group, the preloading strategy is determined from the modification types to minimise memory usage.
4. A new network variant is cloned from the origin variant, and all modification groups are applied sequentially.
5. The build result is published on `build.result`.
6. Cancellation is handled via the `build.cancel` queue — an in-progress `CompletableFuture` is cancelled and a `build.stopped` message is emitted.

---

## Modification Application Model

Modifications can be applied in two modes:

- **Incremental mode**: when a modification is created, moved, or copied, it is immediately applied to one or more target network variants provided in the `ModificationApplicationContext` list. Each context specifies a `networkUuid`, `variantId`, optional excluded modifications, and a `reportUuid`.
- **Build mode**: a full variant is rebuilt from scratch by cloning the origin variant and replaying all active modification groups in order. This is triggered asynchronously via RabbitMQ.

---

## Elasticsearch Indexation

 When modifications are applied, the impacted equipment IDs (created, modified, deleted) are indexed in Elasticsearch as `ModificationApplicationInfos` documents, with a unique ID derived from `modificationUuid` and `networkUuid` (and including the originating `groupUuid`). This enables the search endpoint to find which modifications affected a given equipment across a network, with wildcard full-text matching.

---

