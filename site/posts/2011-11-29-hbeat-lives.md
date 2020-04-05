---
title: HBeat Lives
author: Tim Docker
date: 2011-11-29
tags: [haskell]
---
Reorganising my projects home, I copied in the old documentation for my
[hbeat](http://dockerz.net/twd/hBeat) program. The docs needed some
updating, so I decided to check it all still works ok. Fearing bitrot, I
was pleased and a little suprised to see that on my recently rebuilt
ubuntu machine, all I needed was

    sudo apt-get install libsdl1.2-dev
    sudo apt-get install libsdl-mixer1.2-dev
    cabal-dev install hbeat

Or at least that's what I first thought. The program fired up ok, but
failed to respond to mouse clicks as expected. It turns out that this
was a pre-existing bug - if the screen redraws don't happen fast enough,
hbeat gets further and further behind in it's event processing
eventually ignoring everything. A small code fix (now published to
hackage) causes out-of-date redraw requests to be dropped. But why was I
seeing this problem now? It seems that since I wrote the software,
openGL via SDL seems to have got alot slower. The compositing window
manager (compiz) seems to be the culprit - it's consuming significant
cpu time whilst hbeat is running. Some references to this can be found
[here](http://forums.libsdl.org/viewtopic.php?t=6511&sid=19ba7791909f191ef4959cf13841caec).
I guess there's a downside to all those fancy compositing effects. It's
a shame hbeat is now a fair bit glitchier than it was before. Maybe
sometime I'll look at this, but for now at least it still works.
