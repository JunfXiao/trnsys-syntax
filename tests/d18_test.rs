use std::path::PathBuf;
use trnsys_syntax::error::RError;
use trnsys_syntax::TrnsysFile;


#[test]
fn test_d18_simple_project() -> Result<(), RError> {
    let file = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("Project_Simple.d18");
    let trnsys_file = TrnsysFile::load(file)?;
    println!("{:#?}", trnsys_file.ctx);
    assert_eq!(trnsys_file.ctx.prev_blocks.len(), 18, "Expected 18 block in the file, found {}", trnsys_file.ctx.prev_blocks.len());
    Ok(())
}