Patches welcome!

- If you are only going to bump bounds:
  - If it's really **only bounds**, please simply open an issue (so you'll have a URL to refer to). I have a semi-automated process to make revisions, pull requests only disturb it.
  - If patch includes **source code change** (i.e. I'll need to make a release), and it's a patch to support **newer base/GHC version**:
    - Amend `tested-with` to include that GHC
    - Regenerate `.travis.yml` with `haskell-ci regenerate` (get the latest from [GitHub haskell-ci/haskell-ci](https://github.com/haskell-ci/haskell-ci))

- Don't edit `CHANGELOG.md`, rather include a copyable entry in your pull request description. I often process pull requests in bulk, and everyone editing the `CHANGELOG.md` causes unnecessary conflicts.

- I use [`stylish-haskell`](https://github.com/jaspervdj/stylish-haskell) to format imports. I encourage you to use it too, when contributing.
- General code style is 4 spaces, just look around how it looks, it's not so strict.
