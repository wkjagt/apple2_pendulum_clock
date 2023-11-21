![Clock](https://github.com/wkjagt/apple2_pendulum_clock/assets/327048/134441f7-af43-4853-a183-bfcb9c88860e)

_TL;DR I used a pendulum clock and a sensor to drive a clock program for the Apple II_

# Making a clock for my Apple 2 clone

I was watching [Dave Plummer's video](https://www.youtube.com/watch?v=CfbciNZvg0o) in which he made a clock in 6502 assembly for his Commodore PET. It looked like a fun thing to do on my Apple 2 Plus clone (I'll sometimes refer to it as "the Apple", so I don't have to say "Apple 2 Plus clone" every time). Plus, it would look nice on my amber CRT. However, Dave has a real-time clock in his Commodore, which I don't have one in my Apple. A real-time clock, the way I understand it, does two things:

- It has an oscillator that oscillates at a convenient power of two times per second, often 32,768Hz (Steve Mould has [a really nice video](https://www.youtube.com/watch?v=_2By2ane2I4) explaining why that power of two in particular). This provides you with the "how fast does time go" part.
- It has a chip that actually keeps time based on this frequency, and gives the computer access to register that chops the time information in values that humans understand (hours, minutes etc). Typically these cards have a battery that keeps the oscillator and chip powered up so you don't have to set the time each time you turn the computer on.

I started thinking about how I could do a similar clock for my Apple 2 without such a card. I didn't really care about it keeping time with the computer turned off because I won't rely on this to know what time it is. I mostly just want to see it work. But you can't make a clock program without the computer knowing how long a minute is. The simplest solution would be to have a really really long loop that does nothing for exactly one minute. You'd have to count the cycles, but I thought that was a boring way to go about it. I liked the idea much more of something providing me with well timed interrupts, the way a real time clock could. I just needed to find something that gave me a signal at predictable intervals that could serve as the base frequency for a clock.

## The pendulum clock

While looking for ideas, I realized the solution was right in front of me: the pendulum clock that belonged to my grandparents, hanging on the wall right behind my Apple. The pendulum would make an amazing oscillator, because technically that is exactly what it is. And this particular one has been instrumental to telling the time for decades, so it really made sense to make use of it. Because this is my grand parents' clock, the one I grew up knowing and hearing tick as a kid, I really didn't want to modify it in any way. My first thought was to convert the motion of the pendulum into electrical signals using something like an ultasonic rangefinder.

## The inductive proximity sensor

But then I discovered something that's called an inductive proximity sensor. It's like a miniature metal detector with a very small range. Apparently it's best at detecting iron. I didn't know what the pendulum of my clock was made of, it's kind of copper coloured, but a magnet sticks to it, so there must be some iron in it. I ordered a couple of these sensors on AliExpress, hoping it could detect whatever the pendulum was made of.

These sensors come in different varieties in terms of how to hook them up and how they behave. The two main choices are NPN vs PNP, and NO vs NC (normally open / normally closed). I picked NPN, because it connects the output line of the sensor to ground when activated. For my use case, this makes sense because I want to generate interrupts on the CPU with this, and the 6502 IRQ and NMI lines are active low, meaning the CPU interrupted when those go low. And I picked Normally Open, because I want the interrupts to happen when the pendulum is detected, not when it isn't.

The first thing I did when the sensors arrived, was hook them up to a 9 volt battery. The product description on Ali says it's a 12-24 volt device (something I hadn't noticed when I ordered it), and I read somewhere that it's actually important to give it the right voltage, but the 9 volt battery was lying on my desk, so that was the lowest effort approach to try it out, assuming that too low a voltage wouldn't damage anything. I could always take something like an old PC power supply to try with 12 volt if 9 didn't work.

I had no idea what kind of signal to expect from this sensor. If, for example, it would be an analog or digital signal. In other words, if it would tell me the strength of the field or something, or just that it either saw metal close by or it didn't. And if it was digital, how clean it would be. It turned out to work perfectly with the 9 volt battery. The signal was a very clean and square looking: high when no metal, and then it would drop to 0 volt immediately when I held something like a screwdriver close to it. The fact that it worked with 9 volt gave me hope that it might also work with 5 volt, which would make this project so much simpler, because I could just feed it 5 volt from somewhere on the motherboard, and it would give me a 5 volt signal back that I could use to generate interrupts. When testing this out with a couple of wires connected to some of the pins on an expansion port of the Apple clone, it turned out that 5 volt still worked, but the sensor seems a lot less sensitive, and would only detect metal object when holding them at a couple of millimeters from the sensor's surface. I figured this would be fine. The pendulum has a pretty steady swing, and always swings in the same plane. It would just swing really close to the sensor.

## Getting interrupts

When thinking a bit more about generating interrupts, I realized that maybe this wouldn't be as straightforward as I initially assumed. The IRQ line on the 6502 is level triggered, meaning the CPU will keep running the interrupt handler as long as the interrupt line is low. Normally you'd use an additional chip that interrupts the 6502 and releases the interrupt when the CPU acknowledges the interrupt (something like a 6522 VIA chip). Connecting the sensor directly to the IRQ line would mean the interrupt handler would be called as soon as it returned, and this for the whole duration that the sensor sees the pendulum passing in front of it.

The NMI line is edge triggered, meaning it's triggered once, when the line goes high to low (the falling edge). This sounds perfect for my use case. The signal coming from the sensor is even surprisingly clean. My oscilloscope shows it as one very clean square pulse for each time the pendulum passed in front of the sensor. But there is one flaw with this plan. The NMI line can't be disabled. That's actually the whole purpose of it: it's a Non Maskable Interrupt. The problem is that the ROM in the Apple 2 maps the NMI vector (indirectly) to the machine language monitor, so I wouldn't be able to load my program and change that vector before the pendulum swings over the sensor, and sends the Apple to the monitor. I guess I could hold the pendulum away from the sensor until the program starts, but I don't want to do that.

![Connecting the sensor to the IRQ line](https://github.com/wkjagt/apple2_pendulum_clock/assets/327048/cfa21a5e-d465-4d93-b980-74e3a2d103b3)

## Gating the sensor to not always interrupt

I ended up using one of the four annunciator pins on the game port (simple output pins that can be either low or high) in combination with an OR gate to gate the signal coming from the sensor. When looking at the state of the annunciators when the computer is first turned on, I saw that annunciator 0 and 1 default to low, and 2 and 3 started out high. This was good news, because I needed one that started out as high, so that it could gate the signal from the sensor as soon as the computer was turned on by just using a simple OR gate. In my clock program I set the annunciator low, so the signal from the sensor passes through, and can be used as interrupts. However, I didn't want to connect the output directly to the IRQ line on the CPU, because the OR gate either pulls the line low, or it pulls it high. What I really wanted was an open drain (or open collector?) OR gate, that either pulls low, or disconnects, but I don't think that exists in 74 series logic. I could be wrong though. I did find a 74LS367 in my parts drawer. This is a 3-state buffer so the outputs can either pull high or low when the output enable pin is low, or go into high-impedance state when the output enable pin is high. This actually makes it very easy to connect because I want the output of this buffer to be enabled when the output of the OR gate is low. So I connected the output of the OR gate to the input of a buffer, and to the output enable pin. I connected the output of the buffer to the IRQ line of the CPU.

![L1060325](https://github.com/wkjagt/apple2_pendulum_clock/assets/327048/9a037e0a-ff2b-4102-898e-7c2c5f93cdec)

## A couple of by the ways:

- The Apple 2 has a really elegant way of addressing expansion cards and other IO. The annunciator I am using to gate the sensor signal is set low by addressing `$C05C`. One of my favorite illustrations in Jim Sather's [Understanding The Apple II]([url](https://archive.org/details/understanding_the_apple_ii/)) show why this it's that address::
<img width="602" alt="image" src="https://user-images.githubusercontent.com/327048/283250371-89a2439c-e698-4c44-98f1-866948041e4b.png">

- Apple ][ clone motherboards are cool because they have a couple of small areas of perf board (see photo above), as if they were intended to be modified/expanded. I used this to solder the OR gate and buffer ICs.

## The code

The source code is the same repo as this readme, but in broad strokes is works as follows. It starts by modifying `$03FC` and `$03FD` in RAM. This is the parameter of a jump instruction that the IRQ vector indirectly points to. I replace the address there with address of my own interrupt handler, so that that gets called once for each pendulum swing. In that handler, I increment a counter in the zero page. This counter counts pendulum swings. I had initially hoped that each pendulum swing was exactly one second, but unfortunately it isn't. One minute is actually 95 pendulum swings. (In fact, it's not even exactly 95, but 95 and a bit, so the clock (the one on the Apple, not the physical one) drifts a bit over time). Another thing the handler does is blink the hours/minute separator, to make the clock look more alive. When the tick counter reaches 95, it increments a value in the zero page where I store the minutes part of the current time. If that rolls over, I also increment the hours. Both these values are stored as binary coded decimal, because that makes it easier to draw the clock.

To draw the clock, I first split the value of the hours in a low and high nibble. Since these are BCD, the high nibble is the first digit, and the low nibble is the second digit. Each of these are used as indexes into a table of bitmaps representing each number from 0-9. Looping over 8 bytes of this bitmap, and shifting out one bit at a time for each byte, the "pixels" (really just inverse spaces) are drawn to the screen by writing to the appropriate address in screen memory. The same is then done for the minutes.

All this happens in interrupts. What doesn't happen in interrupts (I guess in what we can call the main loop), is just a simple check for keyboard presses. The only keys that do something useful are `H` and `M`, which increment the hours and minutes respectively, to adjust the time. Any other key exits the program.

Here's a video showing the clock working. Turn on the sound to hear the clock.

https://github.com/wkjagt/apple2_pendulum_clock/assets/327048/968e700a-afae-4624-afd6-d8ac55b7afeb


## A bit about the workflow

I'm a pretty nostalgic person, and I when I first started programming on this computer, I was using Merlin, for the authentic experience. I was pretty opinionated about using this instead of more modern approaches. It's a really slow process however, and I since switched to a more convenient approach, that combines writing code on my laptop, and running it on the actual hardware. I wrote a loader on the Apple (using Merlin), inspired by [this video](https://www.youtube.com/watch?v=AHYNxpqKqwo) by Ben Eater, where he implements the RS-232 protocol in software. I use a similar approach, where I use one of the switch pins on the game port in the Apple as Rx pin. I read that pin in a precisely timed loop, so that each read of the pin happens right in the middle of the 0 or 1 coming in over the RS-232 protocol. I use an FTDI board, which I connect to a USB port on my laptop, and on the other side the Tx pin to the switch pin of the game port on the Apple.

I am using a simplified version of the Xmodem protocol to send (from my laptop, using a Python script) and receive (on the Apple) the data, and store it starting at address `$6000` on the Apple. Once the transfer is complete, it jumps (witn an `RTS`) to `$6000`, so the code that was just transferred is executed. If that code does an `RTS` (return from subroutine) at the end, control is returned to the loader, and it is ready to receive the next transfer. This makes prototyping really fast, because you can quickly try pieces of code, one after the other.
