### Removing sensitive information from commit history

Use [BFG Repo-Cleaner](https://rtyley.github.io/bfg-repo-cleaner/#usage) to remove sensitive files and folders from previous commits. It assumes your current commit is clean so make sure all dirty files are removed from your current repo version. I followed [this guide](https://github.com/IBM/BluePic/wiki/Using-BFG-Repo-Cleaner-tool-to-remove-sensitive-files-from-your-git-repo) to remove an offending folder and its contents from the commit history. 

You can download BFG using the method listed in the guide, but I downloaded it manually. Open the java program to check your options. In Git Bash, enter:
`java -jar ~/YOUR_FILE_PATH/bfg-1-13-0.jar`
and the list of potential commands will come up. The guide uses the --delete-files command, but I used the --delete-folders command. 

You will follow Option A in the guide assuming you've already removed the file/folder in HEAD. 
