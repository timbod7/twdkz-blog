---
title: Homebrew flight simulator pedals
author: Tim Docker
date: 2020-11-23
tags: [programming,3d-printing]
---

I plan to take some gliding lessons, and hope to accelerate my learning by spending some time
on [a simulator][condor2]. But I know this will only be worthwhile if the controls on the sim
are a reasonable approximation of the real thing - you don't flight an aircraft with a keyboard
and mouse. So I need some half decent controls: a joystick and pedals. It's not too hard to get
an old but ok joystick on ebay, but flight sim pedals are expensive and hard to obtain here in
Australia. So I set out to construct some of my own. Like all homebrew projects, this was more
effort than I expected. But I'm happy with the results, and the project let me work on my 3d
design and printing skills, and also some embedded rust development.


# The hardware

A internet search for DIY flight pedals returns plenty of images, but few documented designs. I
copied the simple mechanical arrangement commonly used, and then set out to replicate it with
a combination of aluminium tubing and 3d printed parts. 3d printing is excellent for small
detailed components, but lacks the strength required for larger components, and takes too long
to print them in any case. Hence I find that an approach using off-the-shelf dimensioned metal or
timber from the local harware store, in combination with 3d printing works really well.

The photos below show the physical construction:

![](/posts/2020-11-23-fsim-pedals/IMG_022.JPEG)
![](/posts/2020-11-23-fsim-pedals/IMG_023.JPEG)
![](/posts/2020-11-23-fsim-pedals/IMG_024.JPEG)

Below are a couple of the components (rendered using github's built in stl renderer). You can view the
other stl files in the [project repo][3dprinting]. 

<script src="https://embed.github.com/view/3d/timbod7/fsim-pedals/master/3d-printing/lever-disk.stl"></script>
<script src="https://embed.github.com/view/3d/timbod7/fsim-pedals/master/3d-printing/lever-mount.stl"></script>

[3dprinting]:https://github.com/timbod7/fsim-pedals/tree/master/3d-printing

# The software

The [rust][] programming language intrigues me. I love the idea of a statically typed, modern
programming language that can fit into the niches currently dominated by C/C++. High performance
code is one such niche, but another is the ability to run "bare metal" on cheap microcontrollers.

Hence while a common approach to building a custom USB [HID controller][hid] like this would be
to use an arduino with a [usb joystick library][ajoystick] with code written in C++, I wanted
to use rust, and chose the ubiquitous and cheap ["blue pill"][bluepill] board. Getting an embedded
development environment up and running can be a huge time suck - luckily this board is well supported
in the embedded rust world, with the excellent [blue-pill-quickstart][] github template project.
The project includes enough information to get up and running, and provides links to more detailed
documentation. But at the end of it you've only made it as far as the "hello world" of the embedded
systems: a blinking led.

The requirements are simple, we need:

* Code to configure and manage the USB port so that it acts as a (single axis) HID joystick.
* Code to measure the voltage from the potentiometer using the microcontrollers ADC (analog
  to digital converter), and supply it to the joystick interface

But doing this from first principles would be a mammoth task. The reference manual for the microcontroller
is 1100 pages, with the ADC documentation on pages 215-254, and the USB port on pages 622-652. And 
even if you studied that, you'd still need to read and understand large chunks of the USB specification
to use it. The lesson here is that, even if the embedded computer is tiny, it's still highly complex
and configurable, and just as on larger computers, we will need to leverage the libraries and code of
others if we want to get things working in a reasonable amount of time.

The good news is that the embedded rust community is vibrant and has released a large
collection of [tools and libraries][1]. However, its a small community, and there's only limited examples and tutorials
to act as guides. And this is one place where rust's programming power and expressiveness makes life
harder for learners. In order to work with a wide range of microcontroller architectures, CPUs, and physical
boards, the open source libraries are often quite abstract, and need to be composed in collections
of abstract APIs and concrete implementations.

After research and trial and error I established that the following rust crates would be needed:

* [usb-device][] - abstract API for USB interfaces
* [embedded-hal][] - Hardware Abstraction Layer for ADCs (and many other types of hardware)
* [stm32f1xx-hal][]- implementations of the above abstractions for the cpu on the blue pill
* [usbd-hid][] - protocol for HID USB devices

with the overall project cargo dependencies being:

```
[dependencies]
stm32f1xx-hal = { version = "0.5.2", features = ["rt", "stm32-usbd", "stm32f103" ] }
cortex-m = "0.6"
cortex-m-rt = { version = "0.6.8", features = ["device"] }
panic-semihosting = "0.5.2"
embedded-hal = "0.2.4"
usb-device = "0.2.5"
usbd-serial = "0.1.0"
usbd-hid = "0.4.4"
```

My development strategy was somewhat simplistic - I started with the working blinking led demo, and mutated
it by adding features one at at time, often cutting and pasting example code from elsewhere, and then
incrementally changing it to what I needed. Whilst unsophisticated, this approach was quicker and
easier than getting a deep understanding of each of the libraries I used. A blocker though was that I was unable
to find an example of the usbd-hid library being used to implement a joystick. This meant that I had
to gain sufficient understanding of the USB [HID specification][hiddef] to write the [descriptor][joydesc]
for a joystick.

Ultimately the [entire code][rustsrc] is currently only 160 lines of rust. But it was quite an effort to get there!

# Future plans

[condor2]:https://www.condorsoaring.com/
[rust]:https://www.rust-lang.org/
[hid]:https://en.wikipedia.org/wiki/USB_human_interface_device_class
[ajoystick]:https://github.com/MHeironimus/ArduinoJoystickLibrary
[bluepill]:https://jeelabs.org/article/1649a/
[blue-pill-quickstart]:https://github.com/TeXitoi/blue-pill-quickstart
[st32f1-ref]:https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwjsio6yxJjtAhXkzjgGHczLBdwQFjAAegQIBRAC&url=https%3A%2F%2Fwww.st.com%2Fresource%2Fen%2Freference_manual%2Fcd00171190-stm32f101xx-stm32f102xx-stm32f103xx-stm32f105xx-and-stm32f107xx-advanced-arm-based-32-bit-mcus-stmicroelectronics.pdf&usg=AOvVaw2kF0T1D3TzsgvgnX7fvMku

[usb-device]:https://crates.io/crates/usb-device
[usbd-hid]:https://crates.io/crates/usbd-hid
[embedded-hal]:https://crates.io/crates/embedded-hal
[stm32f1xx-hal]:https://crates.io/crates/stm32f1xx-hal
[joydesc]:https://github.com/timbod7/fsim-pedals/blob/master/software/fs-pedals/src/joystick.rs
[hiddef]:https://www.usb.org/document-library/device-class-definition-hid-111
[rustsrc]:https://github.com/timbod7/fsim-pedals/tree/master/software/fs-pedals/src

[1]:https://github.com/rust-embedded/awesome-embedded-rust

