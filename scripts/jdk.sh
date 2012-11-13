#!/usr/bin/env sh

JDK_URL="http://download.oracle.com/otn-pub/java/jdk/7u9-b05/jdk-7u9-linux-x64.tar.gz"
JDK_DOWNLOAD_PATH="~/Downloads"
JDK_NAME="jdk-7u9-linux-x64"
JDK_FILE=$JDK_NAME".tar.gz"
JDK_INSTALL_PATH="/usr/lib/jvm/jdk1.7.0/"
JDK_EXTRACTED_DIR="jdk1.7.0_09/*"

cd $JDK_DOWNLOAD_PATH && wget -c $JDK_URL
echo "Download completed."

tar -xf $JDK_FILE
echo "Extracted."

sudo mkdir -p $JDK_INSTALL_PATH
sudo mv $JDK_EXTRACTED_DIR $JDK_INSTALL_PATH
echo "Copying finished."

sudo update-alternatives --install "/usr/bin/java" "java" $JDK_INSTALL_PATH"bin/java" 1
sudo update-alternatives --install "/usr/bin/javac" "javac" $JDK_INSTALL_PATH"bin/javac" 1
sudo update-alternatives --install "/usr/bin/javaws" "javaws" $JDK_INSTALL_PATH"bin/javaws" 1
echo "JDK is activated."

mkdir ~/.mozilla/plugins/
ln -s $JDK_INSTALL_PATH"jre/lib/i386/libnpjp2.so" ~/.mozilla/plugins/
echo "Mozilla plugin is activated."
