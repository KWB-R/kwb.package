# Current Development

The following public functions were added:

- getAuthors()
- getCranPackageDatabase()
- getPackageLicences()
- hasGplLicence()
- packageDependenciesByType()
- plotPackageVersions()
- sortedDependencies(): Was private before. The argument "pattern" is no longer
  supported.
- stopIfNotInstalled()

The following public functions were modified:

- packageDependencies: add arguments "db", "which", "verbose"

The following public functions were deleted:

- drawLink(): It was a one-liner and only used once within the package

The following private functions were added:

- allDeps()
- draw_circles()
- map_to_range()
- pathDescription()
- polar_to_xy()
- readDescription()
- remotes_github_pat() -> remotes:::github_pat()
- remotes_parse_deps() -> remotes:::parse_deps()
- remotes_read_dcf() -> remotes:::read_dcf()
- remotes_untar_description() -> remotes:::untar_description()
- seq_rad_len()
- untarDescriptionFromUrl()

The following private functions were modified:

- cranVersions(): add argument "dbg"
- loadDescriptionFromArchiveUrl(): add argument "path"
- loadDescriptionFromWeb(): add arguments "path", "cache"
- readGithubPackageDescription: add argument "destdir"
- stop_(): rename to cleanStop()

Further changes:

- Add Tests
- Add vignette "Tutorial"
- Update GitHub action files (e.g. master -> v2)

# [kwb.package 0.3.0](https://github.com/KWB-R/kwb.package/releases/tag/v0.3.0) <small>2022-06-11</small>

* Harmonise with R package [kwb.pkgbuild](https://kwb-r.github.io/kwb.pkgbuild)
* Move kwb.utils from Suggests to Imports
* Add `installedDependencies()`, 2020-04-28
* Add `compareInstalledVersions()`, 2020-05-11

# kwb.package 0.2.0

* Added a `NEWS.md` file to track changes to the package.

* see https://style.tidyverse.org/news.html for writing a good `NEWS.md`


