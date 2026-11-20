# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [Unreleased]

## [0.1.0] - unreleased

### Added
- `withPod` and `withContainer`: run a pod of containers for as long as an
  action, ready before the action starts, and removed when it ends,
  however it ends.
- Endpoints of three kinds: a Unix socket of the service (`Native`), a TCP
  port bridged to a Unix socket by a container of socat (`Bridged`), and a
  TCP port published on a port of `127.0.0.1` that Podman picks
  (`Published`). A pod with sockets only has no network. The container of
  socat runs only in a pod that needs it: one with a `Bridged` endpoint, or
  one that waits for a `Published` endpoint to accept connections.
- Readiness: an endpoint accepts connections, a line in a log, a command
  that succeeds in a container, or a check of your own, within a deadline.
- Every run has its own pods and its own folder of sockets, so test
  processes run in parallel. The pods and folders of runs that died are
  removed by the next run.
- Images of a registry, pinned by their digest, and images built from a
  recipe, tagged by its hash; each pulled or built once for every process
  of the machine.
- `podmanUsable`, to skip the tests where Podman cannot run, or when
  `PODMAN_TEST_SKIP` is set.
- The last lines of the log of every container on the standard error when a
  pod is not ready or the action fails.

[Unreleased]: https://github.com/jprupp/podman-test/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/jprupp/podman-test/releases/tag/v0.1.0
