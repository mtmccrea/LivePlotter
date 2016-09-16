
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
	// p = LivePlotter(s, f.index, f.numChannels, 4096);
	s.sync;
	// p = LivePlotter(f, 4096, parent: w); //embedded in another view
	p = LivePlotter(f, 4096, name: "live plotting bus " ++ f.index.asString);
	
	// SynthDef(\bufWr, {arg bus; Out.kr(bus, MouseY.kr(-2, 2) ! f.numChannels)}).add;
	SynthDef(\bufWr, {arg bus; Out.kr(bus, MouseY.kr(-2, 2) ! 1)}).add;
	s.sync;
	y.free; y = Synth(\bufWr, [\bus, f.index]);


	s.plotTree;
});
)
g=  Bus.control(s, 3);
	p.bus_(g);
b = Buffer.alloc(s, 200)
b.free
p.rate_(50);

p.overlay_(true);
p.overlay_(false);
p.autoYRange_(true);
p.autoYRange_(false);
p.yRange_(-1, 1);

y.free;
p.free;



//parent class - basic static plotter
//I'm also thinking about extending it into a more complete buffer plotter, with auto bounds, zoom, scrolling etc. The basic class has minMax manually set (-1 to 1 by default), and no zooming. But it's fast!
// s.quit
// s.options.sampleRate_(48000)
(
s.waitForBoot({
	b.free; b = Buffer.alloc(s, s.sampleRate * 10, 1);
	q = ScopePlotter.new(b);

});
)

//fill the buffer incrementally
k = fork{5000.do({|inc| b.set(s.sampleRate * 5 + inc, 0.9); 0.001.wait})}

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

