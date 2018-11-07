Since this blog is built using the excellent [serverless-haskell](https://github.com/seek-oss/serverless-haskell) project, and being a fervent advocate for Haskell and its many philosophies (despite my lack of depth in any areas thereof), here is something that I think would be useful if anybody else is to set up anything similar.

## Rule number 1: dev on windows? no!
One thing that I wasted too much time on, is trying to set up the stack for development on Windows. I got really close - but never quite there. There was some indirect dependency with the word "unix" in its name that refused to work. Maybe the name itself strongly hints that it's not meant for Windows.

Instead I got everything working within an hour or so in Ubuntu. No major problems.

## Do not use the Haskell Platform

I am still confused as to where and when to or not to use the Haskell Platform. There seems to be a gap between how Stack works and how the Platform advocates.

In this case though, stick with Stack is easier - only meaningful change I need to make is to set **resolver: lts-9.1** in the stack.yaml file. "stack install" took care of everything.

## GHC-Mod will eat all your memory!

Yes that's right - unfortunately. Due to some strange cock-up with particular versions above GHC 8.1+, GHC-Mod will end up eating up a lot of memory, and for me, eventually freeze up my Ubuntu virtual machine.

I then switched to [GHCID](https://github.com/ndmitchell/ghcid), much lighter, running on command line, but man that makes life much easier.
