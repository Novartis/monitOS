# How to create a release and push to GitHub

1. Ensure all changes to be released are in `main`.
2. Ensure `NEWS.md` is up to date and that docs are built and up to date.
3. Create release branch and tag. This creates a new branch off main with a particular version and
   cleans up some Novartis-specific content.

   ```bash
   # replace <version> with the version number to release, e.g. 0.1.4
   ./inst/release/make-release.sh main <version>
   ```

4. Review the changes in the release branch.
5. Push tags & branch to Gitlab:

    ```bash
    git push origin release-<version> --tags
    git push --tags
    ```

6. Do the CRAN release from the release branch.
7. Push to Github.