import hashlib
from pathlib import Path

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Fpath
from semgrep.semgrep_interfaces.semgrep_output_v1 import Maven
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Unknown
from semgrep.subproject import find_closest_subproject
from semgrep.subproject import get_display_paths
from semgrep.subproject import ResolvedDependencies
from semgrep.subproject import ResolvedSubproject
from semgrep.subproject import Subproject
from semgrep.subproject import to_stats_output


def create_tmp_file(path: Path):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.touch()


class TestFindClosestSubproject:
    @pytest.mark.quick
    def test_finds_subproject_in_same_directory(
        self, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ):
        lockfile_path = Path("a/b/c/requirements.txt")
        create_tmp_file(tmp_path / lockfile_path)
        extra_lockfile_path = Path("a/b/requirements.txt")
        create_tmp_file(tmp_path / Path(extra_lockfile_path))

        monkeypatch.chdir(tmp_path)

        expected = ResolvedSubproject(
            root_dir=Path("a/b/c"),
            resolution_errors=[],
            dependency_source=out.DependencySource(
                out.ManifestLockfileDependencySource(
                    (
                        out.Manifest(
                            out.ManifestKind(out.RequirementsIn()),
                            out.Fpath("a/b/c/requirements.in"),
                        ),
                        out.Lockfile(
                            out.LockfileKind(out.PipRequirementsTxt()),
                            out.Fpath(str(lockfile_path)),
                        ),
                    )
                ),
            ),
            found_dependencies=ResolvedDependencies.from_atd_dependencies([]),
            ecosystem=Ecosystem(Pypi()),
            resolution_method=out.ResolutionMethod(out.LockfileParsing()),
        )
        extra = [
            ResolvedSubproject(
                root_dir=Path("a/b"),
                resolution_errors=[],
                dependency_source=out.DependencySource(
                    out.ManifestLockfileDependencySource(
                        (
                            out.Manifest(
                                out.ManifestKind(out.RequirementsIn()),
                                out.Fpath("a/b/requirements.in"),
                            ),
                            out.Lockfile(
                                out.LockfileKind(out.PipRequirementsTxt()),
                                out.Fpath(str(extra_lockfile_path)),
                            ),
                        )
                    ),
                ),
                found_dependencies=ResolvedDependencies.from_atd_dependencies([]),
                ecosystem=Ecosystem(Pypi()),
                resolution_method=out.ResolutionMethod(out.LockfileParsing()),
            )
        ]

        assert (
            find_closest_subproject(
                Path("a/b/c/test.py"), Ecosystem(Pypi()), [*extra, expected]
            )
            == expected
        ), "Should return subproject with lockfile in same directory"

    @pytest.mark.quick
    def test_finds_subproject_for_requested_ecosystem(self, tmp_path, monkeypatch):
        lockfile_path = Path("a/b/gradle.lockfile")
        create_tmp_file(tmp_path / lockfile_path)
        extra_lockfile_path = Path("a/b/c/requirement.txt")
        create_tmp_file(tmp_path / Path(extra_lockfile_path))

        monkeypatch.chdir(tmp_path)

        expected = ResolvedSubproject(
            root_dir=Path("a/b"),
            resolution_errors=[],
            dependency_source=out.DependencySource(
                out.ManifestLockfileDependencySource(
                    (
                        out.Manifest(
                            out.ManifestKind(out.RequirementsIn()),
                            out.Fpath("a/b/build.gradle"),
                        ),
                        out.Lockfile(
                            out.LockfileKind(out.GradleLockfile()),
                            out.Fpath(str(lockfile_path)),
                        ),
                    )
                ),
            ),
            found_dependencies=ResolvedDependencies.from_atd_dependencies([]),
            ecosystem=Ecosystem(Maven()),
            resolution_method=out.ResolutionMethod(out.LockfileParsing()),
        )
        extra = [
            ResolvedSubproject(
                root_dir=Path("a/b/c"),
                resolution_errors=[],
                dependency_source=out.DependencySource(
                    out.ManifestLockfileDependencySource(
                        (
                            out.Manifest(
                                out.ManifestKind(out.RequirementsIn()),
                                out.Fpath("a/b/c/requirements.in"),
                            ),
                            out.Lockfile(
                                out.LockfileKind(out.PipRequirementsTxt()),
                                out.Fpath(str(extra_lockfile_path)),
                            ),
                        )
                    ),
                ),
                found_dependencies=ResolvedDependencies.from_atd_dependencies([]),
                ecosystem=Ecosystem(Pypi()),
                resolution_method=out.ResolutionMethod(out.LockfileParsing()),
            )
        ]

        result = find_closest_subproject(
            Path("a/b/c/app/test.java"), Ecosystem(Maven()), [expected, *extra]
        )
        assert result == expected, "Should return subproject with requested ecosystem"


class TestSubproject:
    @pytest.mark.quick
    @pytest.mark.parametrize(
        "lockfile_path", [Path("a/b/c/requirements.txt"), Path("requirements.txt")]
    )
    def test_base_case(self, lockfile_path: Path):
        atd_dependencies = [
            (
                FoundDependency(
                    package="requests",
                    version="2.26.0",
                    ecosystem=Ecosystem(Pypi()),
                    allowed_hashes={},
                    transitivity=Transitivity(Unknown()),
                    lockfile_path=Fpath(str(lockfile_path)),
                ),
                None,
            )
        ]

        subproject = ResolvedSubproject(
            root_dir=Path("a/b/c"),
            resolution_errors=[],
            dependency_source=out.DependencySource(
                out.ManifestLockfileDependencySource(
                    (
                        out.Manifest(
                            out.ManifestKind(out.RequirementsIn()),
                            out.Fpath("a/b/c/requirements.in"),
                        ),
                        out.Lockfile(
                            out.LockfileKind(out.PipRequirementsTxt()),
                            out.Fpath(str(lockfile_path)),
                        ),
                    )
                ),
            ),
            resolution_method=out.ResolutionMethod(out.LockfileParsing()),
            ecosystem=Ecosystem(Pypi()),
            found_dependencies=ResolvedDependencies.from_atd_dependencies(
                atd_dependencies
            ),
        )
        (
            lockfile_dep_map,
            unknown_lockfile_deps,
        ) = subproject.found_dependencies.make_dependencies_by_source_path()
        assert len(unknown_lockfile_deps) == 0
        assert lockfile_dep_map == {
            str(lockfile_path): [d[0] for d in atd_dependencies]
        }, "Should return mapping of lockfile path to dependencies"

        assert get_display_paths(subproject.dependency_source) == [
            lockfile_path
        ], "Should return lockfile path"

    @pytest.mark.quick
    def test_multiple_lockfiles(self):
        lockfile_path = Path("a/b/c/requirements/base.txt")
        extra_lockfile_path = Path("a/b/requirements/dev.txt")
        atd_dependencies = [
            (
                FoundDependency(
                    package="requests",
                    version="2.26.0",
                    ecosystem=Ecosystem(Pypi()),
                    allowed_hashes={},
                    transitivity=Transitivity(Unknown()),
                    lockfile_path=Fpath(str(lockfile_path)),
                ),
                None,
            ),
            (
                FoundDependency(
                    package="flask",
                    version="2.0.0",
                    ecosystem=Ecosystem(Pypi()),
                    allowed_hashes={},
                    transitivity=Transitivity(Unknown()),
                    lockfile_path=Fpath(str(extra_lockfile_path)),
                ),
                None,
            ),
        ]

        multi_lockfile_source = out.DependencySource(
            out.MultiLockfileDependencySource(
                [
                    out.DependencySource(
                        out.LockfileOnlyDependencySource(
                            out.Lockfile(
                                out.LockfileKind(out.PipRequirementsTxt()),
                                out.Fpath(str(lockfile_path)),
                            )
                        )
                    ),
                    out.DependencySource(
                        out.LockfileOnlyDependencySource(
                            out.Lockfile(
                                out.LockfileKind(out.PipRequirementsTxt()),
                                out.Fpath(str(extra_lockfile_path)),
                            )
                        )
                    ),
                ],
            )
        )

        subproject = ResolvedSubproject(
            root_dir=Path("a/b/c"),
            resolution_errors=[],
            dependency_source=multi_lockfile_source,
            ecosystem=Ecosystem(Pypi()),
            resolution_method=out.ResolutionMethod(out.LockfileParsing()),
            found_dependencies=ResolvedDependencies.from_atd_dependencies(
                atd_dependencies
            ),
        )

        (
            lockfile_deps_map,
            unknown_lockfile_deps,
        ) = subproject.found_dependencies.make_dependencies_by_source_path()
        assert len(unknown_lockfile_deps) == 0
        assert lockfile_deps_map[str(lockfile_path)][0] == atd_dependencies[0][0]
        assert lockfile_deps_map[str(extra_lockfile_path)][0] == atd_dependencies[1][0]

        assert get_display_paths(subproject.dependency_source) == [
            lockfile_path,
            extra_lockfile_path,
        ], "Should return lockfile paths"

    @pytest.mark.quick
    def test_dep_missing_lockfile_path(self):
        lockfile_path = Path("requirements.txt")
        atd_dependencies = [
            (
                FoundDependency(
                    package="requests",
                    version="2.26.0",
                    ecosystem=Ecosystem(Pypi()),
                    allowed_hashes={},
                    transitivity=Transitivity(Unknown()),
                ),
                None,
            )
        ]

        subproject = ResolvedSubproject(
            root_dir=Path("a/b/c"),
            resolution_errors=[],
            dependency_source=out.DependencySource(
                out.ManifestLockfileDependencySource(
                    (
                        out.Manifest(
                            out.ManifestKind(value=out.RequirementsIn()),
                            out.Fpath("a/b/c/requirements.in"),
                        ),
                        out.Lockfile(
                            out.LockfileKind(out.PipRequirementsTxt()),
                            out.Fpath(str(lockfile_path)),
                        ),
                    )
                ),
            ),
            resolution_method=out.ResolutionMethod(out.LockfileParsing()),
            ecosystem=Ecosystem(Pypi()),
            found_dependencies=ResolvedDependencies.from_atd_dependencies(
                atd_dependencies
            ),
        )

        (
            lockfile_deps_map,
            unknown_lockfile_deps,
        ) = subproject.found_dependencies.make_dependencies_by_source_path()
        assert len(unknown_lockfile_deps) == 1
        assert len(lockfile_deps_map) == 0

        assert get_display_paths(subproject.dependency_source) == [
            lockfile_path
        ], "Should return lockfile path"

    @pytest.mark.quick
    def test_to_stats_output(self):
        lockfile_src = out.LockfileOnlyDependencySource(
            out.Lockfile(
                out.LockfileKind(out.PipRequirementsTxt()),
                out.Fpath("a/b/c/requirements.txt"),
            ),
        )
        dependency_source = out.DependencySource(lockfile_src)

        subproject = Subproject(
            root_dir=Path("a/b/c"),
            dependency_source=dependency_source,
            ecosystem=Ecosystem(Pypi()),
        )

        subproject_id = hashlib.sha256(
            str(lockfile_src.value.path.value).encode("utf-8")
        ).hexdigest()

        assert subproject.to_stats_output() == out.SubprojectStats(
            subproject_id=subproject_id,
            dependency_sources=to_stats_output(dependency_source),
            resolved_stats=None,
        )


class TestResolvedSubproject:
    @pytest.mark.quick
    def test_to_stats_output(self):
        lockfile_path = Path("a/b/c/requirements.txt")
        dependency_source = out.DependencySource(
            out.LockfileOnlyDependencySource(
                out.Lockfile(
                    out.LockfileKind(out.PipRequirementsTxt()),
                    out.Fpath(str(lockfile_path)),
                ),
            )
        )
        ecosystem = Ecosystem(Pypi())

        subproject = ResolvedSubproject(
            root_dir=Path("a/b/c"),
            resolution_errors=[],
            dependency_source=dependency_source,
            resolution_method=out.ResolutionMethod(out.LockfileParsing()),
            ecosystem=ecosystem,
            found_dependencies=ResolvedDependencies.from_atd_dependencies([]),
        )

        subproject_id = hashlib.sha256(str(lockfile_path).encode("utf-8")).hexdigest()

        assert subproject.to_stats_output() == out.SubprojectStats(
            subproject_id=subproject_id,
            dependency_sources=to_stats_output(dependency_source),
            resolved_stats=out.DependencyResolutionStats(
                resolution_method=out.ResolutionMethod(out.LockfileParsing()),
                dependency_count=0,
                ecosystem=ecosystem,
            ),
        )


class TestLockfileOnlyDependencySource:
    @pytest.fixture
    def lockfile_source(self):
        lockfile_path = Path("a/b/c/requirements.txt")
        return (
            lockfile_path,
            out.LockfileOnlyDependencySource(
                out.Lockfile(
                    out.LockfileKind(out.PipRequirementsTxt()),
                    out.Fpath(str(lockfile_path)),
                )
            ),
        )

    @pytest.mark.quick
    def test_base_case(self, lockfile_source):
        lockfile_path, source = lockfile_source
        assert get_display_paths(out.DependencySource(source)) == [
            lockfile_path
        ], "Should return lockfile path"

    @pytest.mark.quick
    def test_to_stats_output(self, lockfile_source):
        lockfile_path, source = lockfile_source

        assert to_stats_output(out.DependencySource(source)) == [
            out.DependencySourceFile(
                kind=out.DependencySourceFileKind(
                    value=out.Lockfile_(
                        value=out.LockfileKind(out.PipRequirementsTxt())
                    )
                ),
                path=out.Fpath(str(lockfile_path)),
            )
        ]


class TestMultiLockfileDependencySource:
    @pytest.fixture
    def multi_lockfile_source(self):
        lockfile_path = Path("a/b/c/requirements.txt")
        extra_lockfile_path = Path("a/b/requirements/dev.txt")

        source = out.MultiLockfileDependencySource(
            [
                out.DependencySource(
                    out.LockfileOnlyDependencySource(
                        out.Lockfile(
                            out.LockfileKind(out.PipRequirementsTxt()),
                            out.Fpath(str(lockfile_path)),
                        )
                    )
                ),
                out.DependencySource(
                    out.LockfileOnlyDependencySource(
                        out.Lockfile(
                            out.LockfileKind(out.PoetryLock()),
                            out.Fpath(str(extra_lockfile_path)),
                        )
                    )
                ),
            ]
        )

        return (
            lockfile_path,
            extra_lockfile_path,
            source,
        )

    @pytest.mark.quick
    def test_base_case(self, multi_lockfile_source):
        lockfile_path, extra_lockfile_path, source = multi_lockfile_source

        assert get_display_paths(out.DependencySource(source)) == [
            lockfile_path,
            extra_lockfile_path,
        ], "Should return lockfile paths"

    @pytest.mark.quick
    def test_to_stats_output(self, multi_lockfile_source):
        lockfile_path, extra_lockfile_path, source = multi_lockfile_source

        assert to_stats_output(out.DependencySource(source)) == [
            out.DependencySourceFile(
                kind=out.DependencySourceFileKind(
                    value=out.Lockfile_(
                        value=out.LockfileKind(out.PipRequirementsTxt())
                    )
                ),
                path=out.Fpath(str(lockfile_path)),
            ),
            out.DependencySourceFile(
                kind=out.DependencySourceFileKind(
                    value=out.Lockfile_(value=out.LockfileKind(out.PoetryLock()))
                ),
                path=out.Fpath(str(extra_lockfile_path)),
            ),
        ]


class TestManifestOnlyDependencySource:
    @pytest.fixture
    def manifest_source(self):
        manifest_path = Path("a/b/c/pyproject.toml")
        return (
            manifest_path,
            out.ManifestOnlyDependencySource(
                out.Manifest(
                    out.ManifestKind(out.PyprojectToml()),
                    out.Fpath(str(manifest_path)),
                )
            ),
        )

    @pytest.mark.quick
    def test_base_case(self, manifest_source):
        manifest_path, source = manifest_source
        assert get_display_paths(out.DependencySource(source)) == [
            manifest_path
        ], "Should return manifest path"

    @pytest.mark.quick
    def test_to_stats_output(self, manifest_source):
        manifest_path, source = manifest_source
        assert to_stats_output(out.DependencySource(source)) == [
            out.DependencySourceFile(
                kind=out.DependencySourceFileKind(
                    value=out.Manifest_(value=out.ManifestKind(out.PyprojectToml()))
                ),
                path=out.Fpath(str(manifest_path)),
            )
        ]


class TestManifestLockfileDependencySource:
    @pytest.fixture
    def manifest_lockfile_source(self):
        manifest_path = Path("a/b/c/pyproject.toml")
        lockfile_path = Path("a/b/c/poetry.lock")
        return (
            manifest_path,
            lockfile_path,
            out.ManifestLockfileDependencySource(
                (
                    out.Manifest(
                        out.ManifestKind(out.PyprojectToml()),
                        out.Fpath(str(manifest_path)),
                    ),
                    out.Lockfile(
                        out.LockfileKind(out.PoetryLock()),
                        out.Fpath(str(lockfile_path)),
                    ),
                ),
            ),
        )

    @pytest.mark.quick
    def test_base_case(self, manifest_lockfile_source):
        _, lockfile_path, source = manifest_lockfile_source
        assert get_display_paths(out.DependencySource(source)) == [
            lockfile_path
        ], "Should return lockfile path"

    @pytest.mark.quick
    def test_to_stats_output(self, manifest_lockfile_source):
        manifest_path, lockfile_path, source = manifest_lockfile_source
        assert to_stats_output(out.DependencySource(source)) == [
            out.DependencySourceFile(
                kind=out.DependencySourceFileKind(
                    value=out.Lockfile_(value=out.LockfileKind(out.PoetryLock()))
                ),
                path=out.Fpath(str(lockfile_path)),
            ),
            out.DependencySourceFile(
                kind=out.DependencySourceFileKind(
                    value=out.Manifest_(value=out.ManifestKind(out.PyprojectToml()))
                ),
                path=out.Fpath(str(manifest_path)),
            ),
        ]
