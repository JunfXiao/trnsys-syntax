

// pub fn parse_document(input: &str) -> IResult<&str, Vec<Block>, RError> {
//     let (mut input, mut blocks) = many0(parse_commented_block).parse(input)?;
// 
//     // 处理文档尾部注释
//     let (_, trailing_comments) = parse_block_comment(input)?;
//     // if let Some(trailing_comments) = trailing_comments {
//     //     if let Some(last_block) = blocks.last_mut() {
//     //         let mut post_comments = last_block.comments_mut().comment_post.as_mut();
//     //         if post_comments.is_none() {
//     //             drop(post_comments);
//     //             last_block.comments_mut().comment_post = Some(trailing_comments.into_iter().map(Into::into).collect());
//     //         } else {
//     //             post_comments.unwrap().extend(trailing_comments.into_iter().map(Into::into));
//     //         }
//     //     }
//     // }
//     todo!();
//     Ok(("", blocks))
// }
