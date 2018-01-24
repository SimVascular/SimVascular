# macos 10.11 / clang 8.0.0 / homebrew

# add clang compiler
xcode-select --install

# install homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

brew install gcc
brew install cmake
brew install wget
brew cask install emacs
brew install openssl
brew install pcre
brew install bison

# openssl (not sure if this is really needed!)
ln -s /usr/local/opt/openssl /usr/local/openssl
# not sure libgcrypt is needed (doesn't seem to replace
# libcrypt which could be used by python build)
brew install libgcrypt

# now install Qt precompiled

sudo mkdir -p /usr/local/package
# install dmg of qt 5.4.2 into /usr/local/package/Qt5.4.2
wget http://simvascular.stanford.edu/downloads/public/open_source/mac_osx/qt/5.4/qt-opensource-mac-x64-clang-5.4.2.dmg
./qt-opensource-mac-x64-clang-5.4.2.dmg

