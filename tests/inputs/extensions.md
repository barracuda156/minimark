# Extensions

Corpus for Phase 1 markdown constructs added after the initial pipeline.
Accumulates across tasks — add new sections, don't replace old ones.

## Task lists (T1.1)

- [ ] unchecked item
- [x] checked item (lowercase x)
- [X] checked item (uppercase X)
- plain bullet item, no checkbox

1. [ ] first ordered task
2. [x] second ordered task, done
3. plain ordered item

- [x] checked item with continuation
      second line of the same item, indented under the checkbox

## Strikethrough (T1.1)

This is ~~deleted text~~ inside a sentence.

~~Whole line struck through.~~

Mixed **bold and ~~struck~~ together**.

## Reference links, titles, images (T1.2)

Inline link with title: [Anthropic](https://www.anthropic.com "Anthropic home").

Reference link: [the docs][docs-ref] explain more.

Collapsed reference: [Docs Ref][] also works.

Shortcut reference: [Docs Ref] resolves the same way.

Undefined reference stays literal: [nowhere][missing-ref].

Undefined shortcut stays literal: [Also Missing].

Image with title: ![a small logo](https://example.com/logo.png "Logo title")

Reference image: ![alt text][img-ref]

Paren url survives inline: [Haskell](https://en.wikipedia.org/wiki/Haskell_(programming_language)) stays whole.

Paren url by reference: [the Haskell article][wiki-hask].

Paren-form title: [point here][paren-title] uses (title) syntax.

[docs-ref]: https://example.com/docs "Example Docs"
[Docs Ref]: https://example.com/docs-ref
[img-ref]: https://example.com/image.png "An image"
[wiki-hask]: https://en.wikipedia.org/wiki/Haskell_(programming_language)
[paren-title]: https://example.com/pt (Paren Title)

## Math small fixes (T1.5)

Cube root: $\sqrt[3]{8}$. Fourth root: $\sqrt[4]{16}$. Other index: $\sqrt[5]{32}$.

Plain root unaffected: $\sqrt{2}$.

Nested content under a cube root: $\sqrt[3]{x+y}$.
