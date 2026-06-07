# Bulk Updater for Vintage Story Mods

Currently it just updates every mods in your `VintagestoryData/Mods` and writes
a lockfile that caches a local mod database.

- optionally, pass a `--host <host>` to update mods for a specific server.
  - `<host>` is `(<ip-address>|<hostname>)[:<ports>]`, which mirrors to `VintagestoryData/ModsByServer/<host>`
- optionally, set an `$EDITOR` that launches your favorite editor to
  edit the update recipe beforehand so that you can choose what to upgrade
  (or install new ones) and what not to.
