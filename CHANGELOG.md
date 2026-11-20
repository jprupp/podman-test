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
- The Podman of the machine is found in a pure Nix shell, as `stack --nix`
  makes one: in `/usr/bin`, `/usr/local/bin` or NixOS's
  `/run/current-system/sw/bin` when it is not on the `PATH`, or where
  `PODMAN_TEST_PODMAN` says. It runs with the folders of the machine on its
  `PATH`, where it finds its setuid helpers, and without `LD_LIBRARY_PATH`
  or `LD_PRELOAD`. `podmanPath` says which one a run uses.
- The base folder of the sockets is private to the user: made with no
  access for anyone else, closed to others if they could use it, and
  refused with `UnsafeFolder` if it is a link or belongs to another user.
  The folders of the pods, and native sockets, are open to everyone, for
  services that run as another user in their containers, so this folder is
  what keeps the other users of the machine out.
- The last lines of the log of every container on the standard error when a
  pod is not ready or the action fails.

[Unreleased]: https://github.com/jprupp/podman-test/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/jprupp/podman-test/releases/tag/v0.1.0
