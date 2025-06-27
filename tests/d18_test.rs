use serde::{Deserialize, Serialize};
use std::io::Write;
use std::path::PathBuf;
use trnsys_syntax::TrnsysFile;
use trnsys_syntax::error::{Error, RError};
use trnsys_syntax::parse::BlockKind;
use trnsys_syntax::serialize::{DeckBinaryCodec, DeckJsonCodec, DeckWrite};

enum OutputFormat {
    Deck,
    Json,
    Binary,
    Yaml,
}

/// Write the Trnsys Deck File to the `output` subfolder given the name and content.
fn write_to_file<'a, T>(name: &str, content: &T, output_format: OutputFormat) -> Result<(), RError>
where
    T: DeckWrite + DeckJsonCodec<'a> + DeckBinaryCodec<'a> + Serialize + Deserialize<'a>,
{
    let output_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("output");
    std::fs::create_dir_all(&output_dir).map_err(|e| Error::from(e))?;

    let ext = match output_format {
        OutputFormat::Deck => "b18",
        OutputFormat::Json => "json",
        OutputFormat::Binary => "bin",
        OutputFormat::Yaml => "yaml",
    };

    let file_path = output_dir.join(format!("{name}.{ext}"));
    // overwrite the file if it exists
    if file_path.exists() {
        std::fs::remove_file(&file_path).map_err(|e| Error::from(e))?;
    }

    let mut file = std::fs::File::create(file_path).map_err(|e| Error::from(e))?;

    match output_format {
        OutputFormat::Deck => {
            content.write_to_bytes(&mut file, BlockKind::Unknown)?;
            Ok(())
        }
        OutputFormat::Json => {
            let json_content = content.as_json()?;
            file.write_all(json_content.as_bytes())?;
            Ok(())
        }
        OutputFormat::Binary => Err(Error::ConversionError {
            input: "Deck File".to_string(),
            target: "Binary".to_string(),
        }),
        OutputFormat::Yaml => {
            file.write_all(content.as_yaml()?.as_bytes())?;
            Ok(())
        }
    }?;

    file.flush().map_err(|e| Error::from(e))?;

    Ok(())
}

#[test]
fn test_d18_simple_project() -> Result<(), RError> {
    let file = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("Project_Simple.d18");
    let trnsys_file = TrnsysFile::load(file)?;
    println!("{:#?}", trnsys_file.ctx);
    assert_eq!(
        trnsys_file.ctx.prev_blocks.len(),
        19,
        "Expected 19 block in the file, found {}",
        trnsys_file.ctx.prev_blocks.len()
    );
    // TODO: Further checks on the blocks
    write_to_file("Project_Simple", &trnsys_file.ctx, OutputFormat::Deck)?;
    write_to_file("Project_Simple", &trnsys_file.ctx, OutputFormat::Json)?;
    write_to_file("Project_Simple", &trnsys_file.ctx, OutputFormat::Yaml)?;
    // write the file back to disk
    Ok(())
}

#[test]
fn test_d18_complex_project() -> Result<(), RError> {
    let file = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("Project_Complex.d18");
    let trnsys_file = TrnsysFile::load(file)?;
    println!("{:#?}", trnsys_file.ctx);
    assert_eq!(
        trnsys_file.ctx.prev_blocks.len(),
        66,
        "Expected 66 block in the file, found {}",
        trnsys_file.ctx.prev_blocks.len()
    );

    write_to_file("Project_Complex", &trnsys_file.ctx, OutputFormat::Deck)?;
    write_to_file("Project_Complex", &trnsys_file.ctx, OutputFormat::Json)?;
    write_to_file("Project_Complex", &trnsys_file.ctx, OutputFormat::Yaml)?;
    // TODO: Further checks on the blocks
    Ok(())
}
