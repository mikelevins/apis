# ***********************************************************************
#
# Name:          Makefile
# Project:       Apis
# Purpose:       the Apis Cocoa Makefile
# Author:        mikel evins
# Copyright:     2015 by mikel evins
#
# ***********************************************************************

# Which Clozure Common Lisp? (edit for your system)
CCL_DIR=/usr/local/Cellar/clozure-cl/1.10/libexec
LISP=${CCL_DIR}/dx86cl64


APPNAME=Apis
BUNDLE=${APPNAME}.app
BUNDLE_RSRC=./assets/bundle_resources

EXECUTABLE=${APPNAME}.image

all: bundle

clean:
	rm -rf ${BUNDLE}
	find . -name "*.dfsl" -delete
	find . -name "*~" -exec rm -rf {} \;
	find . -name "*.dx??fsl" -delete

tidy:
	find . -name "*.dfsl" -delete
	find . -name "*~" -exec rm -rf {} \;
	find . -name "*.dx??fsl" -delete

bundle: image
	mkdir -p ./${BUNDLE}/Contents/Resources/en.lproj/
	mkdir -p ./${BUNDLE}/Contents/MacOS
	cp ./${BUNDLE_RSRC}/en.lproj/Credits.rtf ./${BUNDLE}/Contents/Resources/en.lproj/Credits.rtf
	cp ./${BUNDLE_RSRC}/en.lproj/InfoPlist.strings ./${BUNDLE}/Contents/Resources/en.lproj/InfoPlist.strings
	cp ./${BUNDLE_RSRC}/Info.plist ./${BUNDLE}/Contents/Info.plist
	cp ./${BUNDLE_RSRC}/Apis.icns ./${BUNDLE}/Contents/Resources/apis.icns
	mv ./${EXECUTABLE} ./${BUNDLE}/Contents/MacOS/${APPNAME}
	find . -name "*.dfsl" -delete
	find . -name "*.dx??fsl" -delete

image: 
	${LISP} --batch -l "build.asd" -e "(build-apis \"${EXECUTABLE}\")"


