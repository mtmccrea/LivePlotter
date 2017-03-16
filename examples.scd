
//live plotting a control bus
//LivePlotter.new(aBusInstance, maxBufSize (practically max window size), name, bounds, parentView)

(//parent optionally
w = Window.new.front;
w.layout = VLayout();
StaticText(w).string_("some text");
)

(
s.waitForBoot({
	f=Bus.control(s, 2);
	s.sync;
	p = LivePlotter(f, 4096, parent: w); //embedded in another view
	// p = LivePlotter(f, 640, name: "live plotting bus " ++ f.index.asString);

	// q = LivePlotter(f)

	SynthDef(\bufWr, {arg bus; Out.kr(bus, MouseY.kr(-2, 2) ! f.numChannels)}).add;
	// SynthDef(\bufWr, {arg bus; Out.kr(bus, MouseY.kr(-2, 2) ! 1)}).add;
	// SynthDef(\bufWr, {arg bus; Out.kr(bus, Impulse.kr(3) ! 1)}).add;
	s.sync;
	y.free; y = Synth(\bufWr, [\bus, f.index]);


	s.plotTree;
});
)
p.synth.updaterSynth.dump
g=  Bus.control(s, 3);
	p.bus_(g);
b = Buffer.alloc(s, 3)
p.synthLive.phasorBus.get({|val| val.postln})
p.synth.updaterSynth.get(\bufPhase, {|val| val.postln})
b.free
p.rate
p.rate_(40); //this is in pixels per second
p.view.bounds.width
p.synth.scopeBufSize
p.synth.scopeBufSize_(p.view.bounds.width)
y.free;

p.overlay_(true);
p.overlay_(false);
p.autoYRange_(true);
p.autoYRange_(false);
p.yRange_(-2, 2);
p.synth.minMaxLagTime

y.free;
p.free;

FreqScope
Scope2


//parent class - basic static plotter
//I'm also thinking about extending it into a more complete buffer plotter, with auto bounds, zoom, scrolling etc. The basic class has minMax manually set (-1 to 1 by default), and no zooming. But it's fast!
// s.quit
// s.options.sampleRate_(48000)
(
s.waitForBoot({
	b.free; b = Buffer.alloc(s, s.sampleRate * 1, 1);
	// b.free; b = Buffer.alloc(s, 4, 1);
	q = ScopePlotter.new(b);

});
)
s.plotTree

//fill the buffer incrementally
k = fork{5000.do({|inc| b.set(s.sampleRate * 0.5 + inc, rrand(0.2, 0.8)); 0.001.wait})}
// b.set(1, 0.9)

//fill the buffer with a sine
b.sine1([1, 1.5], normalize: true, asWavetable: false, clearFirst: true);

//zero out
b.zero

//record to it!
(
fork{
	SynthDef(\recordBuf, {arg bus; RecordBuf.ar(SoundIn.ar(bus), b)}).add;
	s.sync;
	y.free; y = Synth(\recordBuf, [\bus, 0]);
}
)
y.free; //stop recording
b.free; q.quit;//free

