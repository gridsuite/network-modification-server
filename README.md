# Network modification server

[![Actions Status](https://github.com/gridsuite/network-modification-server/workflows/CI/badge.svg)](https://github.com/gridsuite/network-modification-server/actions)
[![Coverage Status](https://sonarcloud.io/api/project_badges/measure?project=org.gridsuite%3Anetwork-modification-server&metric=coverage)](https://sonarcloud.io/component_measures?id=org.gridsuite%3Anetwork-modification-server&metric=coverage)
[![MPL-2.0 License](https://img.shields.io/badge/license-MPL_2.0-blue.svg)](https://www.mozilla.org/en-US/MPL/2.0/)

Server to modify the network based on spring-boot.

Please read [liquibase usage](https://github.com/powsybl/powsybl-parent/#liquibase-usage) for instructions to automatically generate changesets.
After you generated a changeset do not forget to add it to git and in src/resource/db/changelog/db.changelog-master.yml
