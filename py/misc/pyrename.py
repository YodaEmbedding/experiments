import os
import re
import argparse

argsParser = argparse.ArgumentParser(description="pyRename")
argsParser.add_argument(
    "-c",
    "--noconfirmation",
    action="store_true",
    help="Suppress confirmation (if no conflicts)",
)
argsParser.add_argument(
    "-d",
    "--subdirectories",
    action="store_true",
    help="Include subdirectories",
)
argsParser.add_argument(
    "-f", "--find", type=str, action="store", help="Find regex"
)
argsParser.add_argument(
    "-i", "--ignorecase", action="store_true", help="Ignore case"
)
argsParser.add_argument(
    "-p",
    "--path",
    type=str,
    action="store",
    help="Directory (default is current directory)",
)
argsParser.add_argument(
    "-r",
    "--replace",
    type=str,
    action="store",
    help="Replace regex (use \\1, \\2, ... for captures)",
)
argsParser.add_argument(
    "-w", "--nowarnings", action="store_true", help="Suppress warnings"
)
args = argsParser.parse_args()


def getFilesRecursively(root: str):
    return [
        os.path.join(path, file)
        for path, subdirs, files in os.walk(root)
        for file in files
    ]


def getFiles(path: str, ignoreSubdirectories: bool):
    return (
        os.listdir(path) if ignoreSubdirectories else getFilesRecursively(path)
    )


path = args.path if args.path != None else "."
find = args.find if args.find != None else input("Find: ")
replace = args.replace if args.replace != None else input("Replace: ")
regexOptions = re.IGNORECASE if args.ignorecase else 0
fnames = getFiles(path, not args.subdirectories)


def getRenameList(find: str, replace: str, regexOptions=0):
    regex = re.compile(find, regexOptions)
    return [(f, regex.sub(replace, f)) for f in fnames if regex.search(f)]


# Get conflicts in duplicate outputs
def getDuplicateConflicts():
    newNames = [x[1] for x in renameList]
    return [x for x in renameList if newNames.count(x[1]) > 1]


# Find conflicts with existing files
def getExistingConflicts():
    newNames = [x[1] for x in renameList]
    return [f for f in fnames if f in newNames]


def renameFiles():
    [os.rename(x[0], x[1]) for x in renameList]


renameList = getRenameList(find, replace, regexOptions)
duplicateConflicts = getDuplicateConflicts()
existingConflicts = getExistingConflicts()


def prettyPair(pairs, middle):
    return "\n".join([("{0}" + middle + "{1}").format(*x) for x in pairs])


print(
    "\nReplacements:\n"
    + (prettyPair(renameList, " => ") if len(renameList) > 0 else "None")
)

if not args.nowarnings:
    print(
        "\nConflicts (duplicate output):\n"
        + (
            prettyPair(duplicateConflicts, " => ")
            if len(duplicateConflicts) > 0
            else "None"
        )
    )
    print(
        "\nConflicts (with existing files):\n"
        + (
            "\n".join(existingConflicts)
            if len(existingConflicts) > 0
            else "None"
        )
    )

if (
    args.noconfirmation
    or input("\nContinue with rename [Y/N]? ").upper() == "Y"
):
    renameFiles()
