#!/bin/sh

tempFolderName="temp"
projectName="Ulti"
lowercaseProjectName=$(echo $projectName | tr '[:upper:]' '[:lower:]')

mill Ulti.ui.fullOpt
mill Ulti.backend.assembly

cd out/$projectName
echo $tempFolderName
rm -rf $tempFolderName
mkdir $tempFolderName
unzip backend/assembly/dest/out.jar -d ./$tempFolderName/
cp ui/fullOpt/dest/out.js ./$tempFolderName/gbge/ui/generatedJSFiles
cd $tempFolderName
zip -r out .
mv out.zip ../$lowercaseProjectName.jar
