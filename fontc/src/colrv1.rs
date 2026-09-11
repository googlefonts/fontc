//! Reading nanoemoji-style COLRv1 configuration files.

use std::{fs, path::Path};

use fontir::source::EmojiConfig;

use crate::Error;

pub(crate) fn read(path: &Path) -> Result<EmojiConfig, Error> {
    let contents = fs::read_to_string(path).map_err(|source| Error::FileIo {
        path: path.to_owned(),
        source,
    })?;
    toml::from_str(&contents).map_err(|source| Error::ColrV1Config {
        path: path.to_owned(),
        source,
    })
}

#[cfg(test)]
mod tests {
    use std::fs;

    use tempfile::tempdir;

    use super::read;

    #[test]
    fn reads_colrv1_config() {
        let dir = tempdir().unwrap();
        let path = dir.path().join("config.toml");
        fs::write(
            &path,
            r#"
family = "Test Color"
output_file = "test.ttf"
color_format = "glyf_colr_1"
clipbox_quantization = 32

[axis.wght]
name = "Weight"
default = 400

[master.regular]
style_name = "Regular"
srcs = ["regular.svg"]

[master.regular.position]
wght = 400
"#,
        )
        .unwrap();

        let config = read(&path).unwrap();
        assert_eq!(config.family, "Test Color");
        assert_eq!(config.output_file, "test.ttf");
        assert_eq!(config.color_format, "glyf_colr_1");
        assert_eq!(config.clipbox_quantization, 32);
        assert_eq!(config.axis["wght"].default, 400.0);
        assert_eq!(config.master["regular"].srcs, vec!["regular.svg"]);
        assert_eq!(config.master["regular"].position["wght"], 400.0);
    }

    #[test]
    fn rejects_malformed_config() {
        let dir = tempdir().unwrap();
        let path = dir.path().join("config.toml");
        fs::write(&path, "this is not valid TOML =").unwrap();

        assert!(matches!(
            read(&path),
            Err(crate::Error::ColrV1Config { .. })
        ));
    }
}
