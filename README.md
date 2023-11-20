![](https://user-images.githubusercontent.com/327048/283263147-69ba475a-9e32-44dc-a139-e2cfe45e5930.JPG)


I was watching Dave Plummer's video in which he made a clock in 6502 assembly for his Commodore PET. It looked like a fun thing to do on my Apple 2 clone. Plus, it would look nice on my amber CRT. However, Dave has a real-time clock in his Commodore, which I don't have one in my Apple. A real-time clock, the way I understand it, does two things:

- It has an oscillator that oscillates at a convenient power of two times per second, often 32,768Hz (Steve Mould has [a really nice video](https://www.youtube.com/watch?v=_2By2ane2I4) explaining why that power of two in particular). This provides you with the "how fast does time go" part.
- It has a chip that actually keeps time based on this frequency, and gives the computer access to register that chops the time information in values that humans understand (hours, minutes etc). Typically these cards have a battery that keeps the oscillator and chip powered up so you don't have to set the time each time you turn the computer on.

I started thinking about how I could do a similar clock for my Apple 2 without such a card. I didn't really care about it keeping time with the computer turned off because I won't rely on this to know what time it is. I mostly just want to see it work. But you can't make a clock program without the computer knowing how long a minute is. The simplest solution would be to have a really really long loop that does nothing for exactly one minute. You'd have to count the cycles, but I thought that was a boring way to go about it. I liked the idea much more of something providing me with well timed interrupts, the way a real time clock could. I just needed to find something that gave me a signal at predictable intervals that could serve as the base frequency for a clock.

While looking for ideas, I realized the solution was right in front of me: the pendulum clock that belonged to my grandparents, hanging on the wall behind my Apple. The pendulum would make an amazing oscillator, because technically that is exactly what it is. And this particular one has been instrumental to telling the time for decades, so it really made sense to make use of it. Because this is my grand parents' clock, the one I grew up knowing and listening to as a kid, I really didn't want to modify it in any way. My first thought was to convert the motion of the pendulum into electrical signals using something like an ultasonic rangefinder. But then I discovered something that's called an inductive proximity sensor. It's like a miniature metal detector with a very small range. It works best with iron, from what I've read. I didn't know what the pendulum of my clock was made of, it's kind of copper coloured, but a magnet sticks to it, so there's some iron in it. I ordered a couple of these sensors on AliExpress, hoping it could detect whatever the pendulum was made of.

The first thing I did when the sensors arrived, was hook them up to a 9 volt battery. The product description on Ali says it's a 12-24 volt device (something I hadn't noticed when I ordered it), and I read somewhere that it's actually important to give it the right voltage, but the 9 volt battery was lying on my desk, and the closest 12 volt source would be an old PC power supply, so I went with the lowest effort, thinking that too low a voltage wouldn't damage anything, and I could always go for the higher effort 12 volts if 9 didn't work.

These sensors come in different varieties in terms of how to hook them up and how they behave. The two main choices are NPN/PNP, and NO/NC (normally open / normally closed). I picked NPN, because it connects the output line of the sensor with ground when activated. For my use case, this makes sense because I want to generate interrupts on the CPU with this, and the 6502 IRQ and NMI lines are active low, meaning the CPU interrupted when those go low. That was my thinking when I ordered, but when I started to actually think about how to use this sensor, I realized this might not work, because of how interrupts work on the 6502.

The IRQ line on the 6502 is level triggered, meaning the CPU will keep running the interrupt handler as long as the interrupt line is low. Normally you'd use an additional chip that interrupts the 6502 and releases the interrupt when the CPU acknowledges the interrupt. With the IRQ line connected to a sensor, that wouldn't work. In my particular case, this would mean the interrupt handler would be running for the whole duration that the sensor sees the pendulum passing in front of it.

The NMI line is edge triggered, meaning it's triggered once, when the line goes high to low (the falling edge). This sounds perfect for my use case. The signal coming from the sensor is even surprisingly clean. My oscilloscope shows it as one very clean square pulse for each time the pendulum passed in front of the sensor. But there is one flaw with this plan. The NMI line can't be disabled. That's actually the whole purpose of it: it's a Non Maskable Interrupt. The problem is that the ROM in the Apple 2 maps the NMI vector (indirectly) to the machine language monitor, so I wouldn't be able to load my program and change that vector before the pendulum swings over the sensor, and sends the Apple to the monitor. I guess I could hold the pendulum away from the sensor until the program starts, but I don't want to do that. Another thing I could do is gate the pulses from the sensor through some 74 series chip and one of the annunciators on the game port. I might still give that a try, because I feel that how I am currently doing things is cheating a bit.

In my mystery box of old computer parts I found an Apple 2 card with two 6522 chips on it. These chips can do many things but one thing it's really good at is managing interrupts. You can tell it to disable and enable interrupts, look for a rising or falling edge, it has functionality to acknowledge interrupts. So basically all I need. It also has timers, which is why this feels like cheating. I don't need that pendulum, because I can just use the timers on the 6522s. When looking for information on the particar card, I found an article from BYTE magazine, where the author reviewed this card, and did exactly that: he made a clock with the timers. But yeah, I really wanted to use the pendulum now, even with this little caveat.

The Apple 2 has a really elegant way of addressing expansion cards. One of my favorite illustrations in Jim Sather's [Understanding The Apple II]([url](https://archive.org/details/understanding_the_apple_ii/)) is this:

<img width="602" alt="image" src="https://user-images.githubusercontent.com/327048/283250371-89a2439c-e698-4c44-98f1-866948041e4b.png">

It shows how the address lines are used to gradually decode the space above RAM in the 16 bit address space in chunks that get split in smaller and smaller chunks, especially from C000 to C7FF. C100 to C7FF is dedicated to expansion cards, where the second nibble is the index of the expansion card slot your card is in. I put my 6522 card in slot 1 (the second slot from the left.) (Slot number starts at 0), so my card gets mapped to the C100 to C1FF region in memory. The card has two 6522 and one of the chip select lines on each chip is further connected to address line 7, which splits the C100 to C1FF region in two: one chip gets C100 - C17F, and the other gets C180 to C1FF. That last one is the one I used, because it's the one that's furthest to the back of the card, so it's easier to route a cable from outside the computer to it.

I am powering the sensor from the 5 volt line on the expansion card. Thankfully it even works with only 5 volts, but I think the range is a lot smaller. I haven't tested this more, but I had to place the sensor really close to the pendulum for it to "see" it. It's kinda hard to get the pendulum going now without it bumping into the sensor, but once it's moving in a straight line, it doesn't touch the sensor. I checked this configuration with the oscilloscope, and it produces very nice and clean 5 volt pulses.

Most of my code is doing the same thing as Dave's (the initial inspiration), so it's not that interesting to talk about it, since he already does that really well in his video. What I needed to do differently was "read the pendulum sensor". The first thing my code does is change the NMI vector:

```
    lda #<nmi_handler
    sta $03fc
    lda #>nmi_handler
    sta $03fd
```

$03fc/d is the parameter of a jump instruction that the NMI vector points to. I am using the NMI vector by the way, and not the IRQ vector, because the 6522 I chose to use is connected to it. The other 6522 is connected to IRQ. For my sue case it doesn't really matter. I replace that address with address of my own nmi handler, so that that gets called once for each pendulum swing. In that handler, I increment a number in the zero page. This counter counts pendulum swings. I had initially hoped that each pendulum swing was exactly one second, but unfortunately it isn't. One minute is actually 96 pendulum swings. (In fact, it's not even exactly 96, but 96 and a bit, so the clock (the one on the Apple, not the physical one) drifts a bit over time). Another thing the handler does is blink the hours/minute separator, to make the clock look more alive.

https://github.com/wkjagt/apple2_pendulum_clock/assets/327048/968e700a-afae-4624-afd6-d8ac55b7afeb
(Don't follow my channel please, I'm not a YouTuber)


It's really nice to see the clock working on the amber CRT screen, and hear the ticks of the clock in sync with the blinking hours/minutes separator. If you pay close attention, you'll see the blinking of the LED on the sensor is when the pendulum is in the center of its movement, but the ticks are at the extremities. And yet the blinking happens at the same time as the clocks. I added a delay in my NMI handler for this, because it looked better when the ticks and the blinking happened at the same time. And since I clear the interrupt on the 6522 at the end of the NMI handler, the delay also serves as a kind of debouncing if for whatever reason the pendulum passed in front of the sensor in a way that it registers twice (for example when the sensor only barely sees it, and it's right on the limit of  A funny (but totally logical) detail to see happen is that the clock on the computer stops when I stop the pendulum.
