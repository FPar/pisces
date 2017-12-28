#!/bin/sh
if [ ! -f $HOME/llvm-build-${LLVM_VER}/bin/llvm-config ]; then
  travis_retry curl https://cmake.org/files/v3.9/cmake-3.9.2-Linux-x86_64.tar.gz | tar -xzf - -C $HOME
  export PATH=$HOME/cmake-3.9.2-Linux-x86_64/bin:$PATH
  travis_retry curl -L https://github.com/ninja-build/ninja/releases/download/v1.7.2/ninja-linux.zip -o ninja-linux.zip
  unzip ninja-linux.zip -d $HOME/bin
  travis_retry curl http://releases.llvm.org/${LLVM_VER}/llvm-${LLVM_VER}.src.tar.xz | tar -xJf - -C $HOME
  rsync -ac $HOME/llvm-${LLVM_VER}.src/ $HOME/llvm-src-${LLVM_VER}
  cd $HOME/llvm-src-${LLVM_VER}
  mkdir -p build && cd build
  cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$HOME/llvm-build-${LLVM_VER} -DLLVM_PARALLEL_LINK_JOBS=1 -DLLVM_TARGETS_TO_BUILD=X86 -DLLVM_BUILD_LLVM_DYLIB=True -DLLVM_LINK_LLVM_DYLIB=True -GNinja ..
  ninja -j4 install
fi
