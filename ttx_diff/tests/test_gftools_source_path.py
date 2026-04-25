"""Tests for source_for_gftools_single_source path computation.

Verifies that --experimental-single-source gets the correct value for
various source/config path layouts. The value must match the source entry
in the config file, which is always relative to the config file itself.
"""

import os
from pathlib import Path

from ttx_diff.core import source_for_gftools_single_source


def _make_repo(tmp_path: Path, name: str, use_git: bool = True) -> Path:
    """Create a fake repo directory with .git marker."""
    repo = tmp_path / name
    repo.mkdir(parents=True, exist_ok=True)
    if use_git:
        (repo / ".git").mkdir()
    return repo


def _touch(path: Path) -> Path:
    """Create a file and its parent directories."""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.touch()
    return path


# -- Same-repo: source in same directory as config --


class TestSameDir:
    """Config and source in the same directory (most common pattern)."""

    def test_sources_dir(self, tmp_path):
        """e.g. Tourney: Sources/config.yaml + Sources/Tourney.glyphs"""
        repo = _make_repo(tmp_path, "Tourney")
        source = _touch(repo / "Sources" / "Tourney.glyphs")
        config = _touch(repo / "Sources" / "config.yaml")
        assert source_for_gftools_single_source(source, config) == "Tourney.glyphs"

    def test_sources_dir_designspace(self, tmp_path):
        """e.g. 42dot-Sans: sources/config.yaml + sources/42dotSans.designspace"""
        repo = _make_repo(tmp_path, "42dot-Sans")
        source = _touch(repo / "sources" / "42dotSans.designspace")
        config = _touch(repo / "sources" / "config.yaml")
        assert (
            source_for_gftools_single_source(source, config) == "42dotSans.designspace"
        )

    def test_config_at_repo_root(self, tmp_path):
        """e.g. nata-sans: config.yaml + NataSans.glyphs at repo root"""
        repo = _make_repo(tmp_path, "nata-sans")
        source = _touch(repo / "NataSans.glyphs")
        config = _touch(repo / "config.yaml")
        assert source_for_gftools_single_source(source, config) == "NataSans.glyphs"

    def test_other_dir_name(self, tmp_path):
        """e.g. Jaini: Source/jaini.yaml + Source/Jaini.glyphs"""
        repo = _make_repo(tmp_path, "Jaini")
        source = _touch(repo / "Source" / "Jaini.glyphs")
        config = _touch(repo / "Source" / "jaini.yaml")
        assert source_for_gftools_single_source(source, config) == "Jaini.glyphs"

    def test_config_in_sources_subdir(self, tmp_path):
        """e.g. Sofia-Sans: sources/Condensed/sofia-cond.yml + sources/Condensed/SofiaSans.glyphs"""
        repo = _make_repo(tmp_path, "Sofia-Sans")
        source = _touch(repo / "sources" / "Condensed" / "SofiaSans.glyphs")
        config = _touch(repo / "sources" / "Condensed" / "sofia-cond.yml")
        assert source_for_gftools_single_source(source, config) == "SofiaSans.glyphs"


# -- Same-repo: source in subdirectory relative to config --


class TestSubdirectory:
    """Source is in a subdirectory relative to the config file."""

    def test_generated_subdir(self, tmp_path):
        """e.g. Cormorant: sources/config-garamond.yaml + sources/generated/CormorantGaramond.glyphs"""
        repo = _make_repo(tmp_path, "Cormorant")
        source = _touch(repo / "sources" / "generated" / "CormorantGaramond.glyphs")
        config = _touch(repo / "sources" / "config-garamond.yaml")
        result = source_for_gftools_single_source(source, config)
        assert result == os.path.join("generated", "CormorantGaramond.glyphs")

    def test_glyphs_subdir(self, tmp_path):
        """e.g. Exo-2.0: sources/config.yaml + sources/Glyphs/Exo2.glyphs"""
        repo = _make_repo(tmp_path, "Exo-2.0")
        source = _touch(repo / "sources" / "Glyphs" / "Exo2.glyphs")
        config = _touch(repo / "sources" / "config.yaml")
        result = source_for_gftools_single_source(source, config)
        assert result == os.path.join("Glyphs", "Exo2.glyphs")

    def test_masters_subdir(self, tmp_path):
        """e.g. Anek: sources/AnekDevanagari/builder.yaml + sources/AnekDevanagari/Masters/Anek.designspace"""
        repo = _make_repo(tmp_path, "Anek")
        source = _touch(
            repo
            / "sources"
            / "AnekDevanagari"
            / "Masters"
            / "AnekDevanagari.designspace"
        )
        config = _touch(repo / "sources" / "AnekDevanagari" / "builder.yaml")
        result = source_for_gftools_single_source(source, config)
        assert result == os.path.join("Masters", "AnekDevanagari.designspace")

    def test_temp_subdir(self, tmp_path):
        """e.g. Blaka: sources/blakaregular.yaml + sources/temp/Blaka-Regular.glyphs"""
        repo = _make_repo(tmp_path, "Blaka")
        source = _touch(repo / "sources" / "temp" / "Blaka-Regular.glyphs")
        config = _touch(repo / "sources" / "blakaregular.yaml")
        result = source_for_gftools_single_source(source, config)
        assert result == os.path.join("temp", "Blaka-Regular.glyphs")

    def test_master_ufo_subdir(self, tmp_path):
        """e.g. geologica: sources/config.yaml + sources/master_ufo/Geologica.designspace"""
        repo = _make_repo(tmp_path, "geologica")
        source = _touch(repo / "sources" / "master_ufo" / "Geologica.designspace")
        config = _touch(repo / "sources" / "config.yaml")
        result = source_for_gftools_single_source(source, config)
        assert result == os.path.join("master_ufo", "Geologica.designspace")


# -- Same-repo: source uses ../ parent-relative path --


class TestParentRelative:
    """Source is in a sibling directory, referenced via ../ from the config."""

    def test_builder_to_sources(self, tmp_path):
        """e.g. Baloo2: builder/Baloo2.yaml + sources/Baloo2.glyphs (config says ../sources/Baloo2.glyphs)"""
        repo = _make_repo(tmp_path, "Baloo2-Variable")
        source = _touch(repo / "sources" / "Baloo2.glyphs")
        config = _touch(repo / "builder" / "Baloo2.yaml")
        result = source_for_gftools_single_source(source, config)
        assert result == os.path.join("..", "sources", "Baloo2.glyphs")

    def test_source_dir_to_sibling(self, tmp_path):
        """e.g. Hubballi: source/builder.yaml + source/Hubballi-Regular.glyphs (via ../source/)"""
        repo = _make_repo(tmp_path, "Hubballi")
        # In the actual repo, the source is referenced as ../source/Hubballi-Regular.glyphs
        # but that resolves to source/Hubballi-Regular.glyphs which is alongside the config
        source = _touch(repo / "source" / "Hubballi-Regular.glyphs")
        config = _touch(repo / "source" / "builder.yaml")
        result = source_for_gftools_single_source(source, config)
        assert result == "Hubballi-Regular.glyphs"

    def test_source_subdir_to_other_subdir(self, tmp_path):
        """e.g. Exile: Source/config.yaml + Source/Source-Glyphs/Exile.glyphs"""
        repo = _make_repo(tmp_path, "Exile")
        source = _touch(repo / "Source" / "Source-Glyphs" / "Exile.glyphs")
        config = _touch(repo / "Source" / "config.yaml")
        result = source_for_gftools_single_source(source, config)
        assert result == os.path.join("Source-Glyphs", "Exile.glyphs")


# -- External config: config from a different repo (e.g. google/fonts) --


class TestExternalConfig:
    """Config is from a different repo (e.g. google/fonts) and gets moved to source repo root."""

    def test_source_at_repo_root(self, tmp_path):
        """External config, source file at repo root."""
        source_repo = _make_repo(tmp_path, "source_repo")
        config_repo = _make_repo(tmp_path, "config_repo")
        source = _touch(source_repo / "MyFont.glyphs")
        config = _touch(config_repo / "ofl" / "myfont" / "config.yaml")
        result = source_for_gftools_single_source(source, config)
        assert result == "MyFont.glyphs"

    def test_source_in_sources_dir(self, tmp_path):
        """External config, source in sources/ subdirectory."""
        source_repo = _make_repo(tmp_path, "source_repo")
        config_repo = _make_repo(tmp_path, "config_repo")
        source = _touch(source_repo / "sources" / "MyFont.glyphs")
        config = _touch(config_repo / "ofl" / "myfont" / "config.yaml")
        result = source_for_gftools_single_source(source, config)
        assert result == os.path.join("sources", "MyFont.glyphs")

    def test_shared_filename_disambiguated(self, tmp_path):
        """e.g. halant: external config, styles/{Bold,Light,...}/font.ufo all share filename."""
        source_repo = _make_repo(tmp_path, "halant")
        config_repo = _make_repo(tmp_path, "google_fonts")

        bold = _touch(source_repo / "styles" / "Bold" / "font.ufo" / "metainfo.plist")
        light = _touch(source_repo / "styles" / "Light" / "font.ufo" / "metainfo.plist")
        config = _touch(config_repo / "ofl" / "halant" / "config.yaml")

        bold_source = bold.parent  # the .ufo directory
        light_source = light.parent

        bold_result = source_for_gftools_single_source(bold_source, config)
        light_result = source_for_gftools_single_source(light_source, config)

        assert bold_result == os.path.join("styles", "Bold", "font.ufo")
        assert light_result == os.path.join("styles", "Light", "font.ufo")
        assert bold_result != light_result  # must disambiguate

    def test_source_in_masters_dir(self, tmp_path):
        """External config, source in masters/ subdirectory."""
        source_repo = _make_repo(tmp_path, "source_repo")
        config_repo = _make_repo(tmp_path, "config_repo")
        source = _touch(source_repo / "masters" / "MyFont.designspace")
        config = _touch(config_repo / "ofl" / "myfont" / "config.yaml")
        result = source_for_gftools_single_source(source, config)
        assert result == os.path.join("masters", "MyFont.designspace")


# -- External config with crater-style SHA directory names --


class TestCraterShaRepos:
    """Repos cloned by crater have _SHA suffix (e.g. halant_5991cb7b8f)."""

    def test_sha_suffix_repo(self, tmp_path):
        """find_repo_root detects dirs with appended SHA as repo roots."""
        cache = tmp_path / ".fontc_crater_cache"
        source_repo = cache / "itfoundry" / "halant_5991cb7b8f"
        source_repo.mkdir(parents=True)
        # No .git, but has SHA suffix -> treated as repo root

        config_repo = cache / "google" / "fonts"
        (config_repo / ".git").mkdir(parents=True)

        source = _touch(source_repo / "styles" / "Bold" / "font.ufo" / "metainfo.plist")
        config = _touch(config_repo / "ofl" / "halant" / "config.yaml")

        result = source_for_gftools_single_source(source.parent, config)
        assert result == os.path.join("styles", "Bold", "font.ufo")

    def test_sha_suffix_same_repo(self, tmp_path):
        """Same-repo case where the repo dir has a SHA suffix (no .git)."""
        cache = tmp_path / ".fontc_crater_cache"
        repo = cache / "Etcetera-Type-Co" / "Tourney_643f1026ad"
        repo.mkdir(parents=True)
        # SHA suffix -> treated as repo root

        source = _touch(repo / "Sources" / "Tourney.glyphs")
        config = _touch(repo / "Sources" / "config.yaml")

        result = source_for_gftools_single_source(source, config)
        assert result == "Tourney.glyphs"


# -- Edge cases --


class TestEdgeCases:
    """Edge cases and regressions."""

    def test_source_name_only_was_ambiguous(self, tmp_path):
        """Regression: source.name would match multiple config entries."""
        repo = _make_repo(tmp_path, "myrepo")
        s1 = _touch(repo / "styles" / "Bold" / "font.ufo" / "x")
        s2 = _touch(repo / "styles" / "Regular" / "font.ufo" / "x")
        config = _touch(repo / "config.yaml")

        r1 = source_for_gftools_single_source(s1.parent, config)
        r2 = source_for_gftools_single_source(s2.parent, config)

        # Both would have been "font.ufo" with source.name
        assert r1 != r2
        assert r1 == os.path.join("styles", "Bold", "font.ufo")
        assert r2 == os.path.join("styles", "Regular", "font.ufo")

    def test_dot_slash_prefix_in_config(self, tmp_path):
        """e.g. science-gothic: ./UFO/ScienceGothic.designspace"""
        repo = _make_repo(tmp_path, "science-gothic")
        source = _touch(repo / "sources" / "UFO" / "ScienceGothic.designspace")
        config = _touch(repo / "sources" / "build-config.yaml")
        result = source_for_gftools_single_source(source, config)
        # os.path.relpath never produces ./ prefix, just "UFO/..."
        assert result == os.path.join("UFO", "ScienceGothic.designspace")

    def test_deeply_nested_config(self, tmp_path):
        """Config in a deeply nested subdirectory."""
        repo = _make_repo(tmp_path, "myrepo")
        source = _touch(repo / "a" / "b" / "c" / "font.glyphs")
        config = _touch(repo / "a" / "b" / "c" / "config.yaml")
        assert source_for_gftools_single_source(source, config) == "font.glyphs"

    def test_multiple_configs_same_source(self, tmp_path):
        """Multiple config files pointing to the same source should all work."""
        repo = _make_repo(tmp_path, "myrepo")
        source = _touch(repo / "sources" / "MyFont.glyphs")
        config1 = _touch(repo / "sources" / "config.yaml")
        config2 = _touch(repo / "sources" / "config-italic.yaml")

        assert source_for_gftools_single_source(source, config1) == "MyFont.glyphs"
        assert source_for_gftools_single_source(source, config2) == "MyFont.glyphs"
