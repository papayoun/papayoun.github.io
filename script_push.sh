# Remove the history
rm -rf .git

# Recreate the repos from the current content only
git init
git add .
git commit -m "Initial commit"

# push to the github remote repos ensuring you overwrite history
git remote add origin https://github.com/papayoun/papayoun.github.io
git push -u --force origin master
