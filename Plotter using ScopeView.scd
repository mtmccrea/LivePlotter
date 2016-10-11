//boot first
s.boot;
//setup window

(
~numCh = 2;
~bufferSize = 4096;
w = Window.new.front;
w.layout = HLayout();
v = ScopeView(w);
v.bounds;
b = ScopeBuffer.alloc(s, ~numCh); //buffer for shared memory scoping
v.server = s;
// v.canFocus = true;
v.bufnum = b.bufnum;
v.background = Color.white;
v.waveColors = Color.rand ! ~numCh;
v.fill = false;
v.start;
//also we need some buffers
c = Buffer.alloc(s, ~bufferSize, ~numCh );
d = Buffer.alloc(s, ~bufferSize, ~numCh );
//and a bus - this one will be plotted
f=Bus.control(s, 2);
)
c.bufnum

//set up SynthDef
(
SynthDef(\scopeTest, { arg scopeBuf = 0, memoryBuf = 0, slidingBuf = 0, inbus = 0, rate = 0.1, bufSize = 100;
	var inSig, memPhasor, slidingSig, slidingPhasor, scopeRdPhasor, o;
	inSig = In.kr(inbus, ~numCh );
	bufSize = bufSize.min(BufFrames.kr(memoryBuf));
	// memPhasor = Phasor.kr(0, rate, 0, BufFrames.kr(memoryBuf));
	memPhasor = Phasor.kr(Changed.kr(bufSize), rate, 0, bufSize);
	BufWr.kr(inSig, memoryBuf, memPhasor);
	// slidingPhasor = Phasor.ar(0, 1, 0, BufFrames.kr(memoryBuf));
	slidingPhasor = Phasor.ar(Changed.kr(bufSize), 1, 0, bufSize);
	slidingSig = BufRd.ar(~numCh , memoryBuf, slidingPhasor);
	// BufWr.ar(slidingSig, slidingBuf, slidingPhasor - K2A.ar(memPhasor));
	BufWr.ar(slidingSig, slidingBuf, (slidingPhasor - memPhasor) % bufSize);
	// ScopeOut2.kr(BufRd.kr(1, slidingBuf, Phasor.kr(0, 1, 0, BufFrames.kr(memoryBuf))), scopeBuf, BufFrames.kr(memoryBuf), BufFrames.kr(memoryBuf));
	// ScopeOut2.kr(BufRd.kr(1, slidingBuf, Phasor.kr(0, 1, 0, bufSize)), scopeBuf, BufFrames.kr(memoryBuf), bufSize);
	// ScopeOut2.ar(BufRd.ar(1, slidingBuf, Phasor.ar(0, 1, 0, BufFrames.kr(memoryBuf))), scopeBuf, BufFrames.kr(memoryBuf), BufFrames.kr(memoryBuf)); //ar
	// ScopeOut2.ar(BufRd.ar(~numCh, slidingBuf, Phasor.ar(0, 1, 0, bufSize)), scopeBuf, BufFrames.kr(memoryBuf), bufSize); //ar
	// ScopeOut2.ar(BufRd.ar(1, slidingBuf, Phasor.ar(o = (Changed.kr(bufSize) * (bufSize < LastValue.kr(bufSize))), 1, 0, bufSize)), scopeBuf, BufFrames.kr(memoryBuf), bufSize); //ar
	// ScopeOut2.ar(BufRd.ar(1, slidingBuf, Phasor.ar(o = (Changed.ar(K2A.ar(bufSize)) * (K2A.ar(bufSize) < LastValue.ar(bufSize))), 1, 0, bufSize)), scopeBuf, BufFrames.kr(memoryBuf), bufSize); //ar
	// scopeRdPhasor = Phasor.ar(Changed.kr(bufSize) * (bufSize < LastValue.kr(bufSize)), 1, 0, bufSize);
	// scopeRdPhasor = Phasor.ar(o = (Changed.ar(K2A.ar(bufSize)) * (K2A.ar(bufSize) < LastValue.ar(bufSize))), 1, 0, bufSize);
	// scopeRdPhasor = Phasor.ar(1, 1, 0, bufSize);
	// scopeRdPhasor = Phasor.ar(Changed.kr(bufSize), 1, 0, bufSize);
	// ScopeOut2.ar(BufRd.ar(1, slidingBuf, scopeRdPhasor, 0), scopeBuf, BufFrames.kr(memoryBuf), Latch.ar(K2A.ar(bufSize), BinaryOpUGen('==', scopeRdPhasor, DC.ar(0)))); //ar
	// ScopeOut2.ar(BufRd.ar(1, slidingBuf, scopeRdPhasor), scopeBuf, bufSize, bufSize); //ar
	// Out.ar(32, o);
	// o.poll;
	// ScopeOut2.ar(BufRd.ar(1, slidingBuf, Phasor.ar(1, 1, 0, bufSize)), scopeBuf, bufSize, bufSize); //ar
	// ScopeOut2.ar(BufRd.ar(1, slidingBuf, Phasor.ar(0, 1, 0, BufFrames.kr(memoryBuf)) % bufSize), scopeBuf, bufSize, bufSize); //ar
}).add;
SynthDef(\updater, {arg slidingBuf = 0, scopeBuf = 0, bufSize = 100;
	ScopeOut2.ar(
		BufRd.ar(~numCh, slidingBuf, Phasor.ar(0, 1, 0, bufSize)),
		scopeBuf,  //shared buffer
		BufFrames.kr(slidingBuf), //maxSize
		bufSize//current size

	); //ar
}).add;
)
Stethoscope
s.quit
///scopeView.setProperty( \updateInterval, rate ) //try this
//also scopeView.y for offset and scopeView.yZoom for scale! NOPE, scopeView.y doesn't work!!!!!
Stethoscope
ScopeView
s.scope
x.free; x = Synth(\scopeTest, [\scopeBuf, b.bufnum, \memoryBuf, c.bufnum, \slidingBuf, d.bufnum, \inbus, f, \rate, 0.1, \bufSize, c.numFrames]);
u.free; u = Synth(\updater, [\scopeBuf, b.bufnum, \slidingBuf, d.bufnum, \bufSize, v.bounds.width]);
x.set(\rate, 0.2); //change speed
x.set(\bufSize, 2048)
x.set(\bufSize, c.numFrames)
x.set(\bufSize, 512)
x.free; x = Synth(\scopeTest, [\scopeBuf, b.bufnum, \memoryBuf, c.bufnum, \slidingBuf, d.bufnum, \inbus, f, \rate, 0.1, \bufSize, v.bounds.width]);
x.set(\bufSize, 342)
x.set(\bufSize, 600)
x.set(\bufSize, 128)
x.set(\bufSize, 100)
x.free; u.free;
x.run(false)
x.run(true)
d.plot
v.style = 0
v.onResize_({x.set(\bufSize, v.bounds.width.clip(0, c.numFrames)); u.set(\bufSize, v.bounds.width.clip(0, c.numFrames))})
v.yZoom = 1
v.y = 0 //doesn't work
v.getProperty( \updateInterval)
v.setProperty( \updateInterval, 20 )

//manually set bus values
f.set(0.5);
f.set(-0.1);

//move ScopeOut to  a separate synth; restart it after removing (but keep circular running!)

//set bus values from MouseY
SynthDef(\bufWr, {arg bus; Out.kr(bus, MouseY.kr(-2, 2) ! ~numCh)}).add;
y = Synth(\bufWr, [\bus, f.index]);
y.free;

//free[b, c, d, f, x, y].do(_.free);

//--------
//class tests
s.boot;
f=Bus.control(s, 2);
// p = LivePlotter(s, f.index, f.numChannels, 4096);
p = LivePlotter(f, 4096);
p.dump
x = p.synths.slidingBuffer.plot
p.synths.synthUpdater.get(\bufSize, {|val|val.postln})
p.synths.synthUpdater.set(\bufSize, 120)
p.synths.synthUpdater.free;
p.synths.synthDefUpdater
p.bus_(f)
p.buffer_(p.synthLive.buffer)
p.synthLive.buffer
x.parent.close
p.buffer.plot
.plot
p.synths.synthMain.set(\autoMinMax, 0)
p.synths.synthMain.set(\minMax, [-1, 1])
p.synths.synthMain.get(\minMax, {|val|val.postln})
p.synth.autoMinMax_(0)
p.synth.autoMinMax_(1)
p.overlay_(true);
p.overlay_(false);
p.autoRangeY_(true);
p.autoRangeY_(false);
p.synths.setMinMax(-1, 0)
p.synths.minMaxLagTime_(2)
p.synths.synthUpdater.trace
p.synths.autoMinMax
p.synths.rate_(0.1)
p.rate_(512)
p.scopeView.setProperty( \updateInterval, 50 )
p.synthLive.updaterSynth.get(\rate, {|val|a = val.postln})
p.synth.updaterSynth.get(\bufSize, {|val|a = val.postln})
p.width
p.synth.scopeBufSize_(p.width)

(
q = Window.new;
r = Routine.run({
	inf.do({
		x = p.synths.slidingBuffer.plot(parent: q);
		0.2.wait;
		x.parent.close;
	});
}, clock: AppClock)
)
		x.parent.close;


s.plotTree

SynthDef(\bufWr, {arg bus; Out.kr(bus, MouseY.kr(-2, 2) ! f.numChannels)}).add;
y = Synth(\bufWr, [\bus, f.index]);
y.free;

(
SynthDef(\trigtest, {arg t_trig = 1;
	Out.kr(32, EnvGen.kr(Env([0, 1, 0], [0, 1]), t_trig).roundUp);
}).add
)
x = Synth(\trigtest)
s.scope
x.set(\t_trig, 1)

x.free

//LivePlotter notes
//should probably recreate buffer after resizing to only find bounds for correct ranges

(
s.waitForBoot {
	f=Bus.control(s, 2);
	SynthDef(\bufWr, {arg bus; Out.kr(bus, MouseY.kr(-2, 2) ! f.numChannels)}).add;
	"presync".postln;
	s.sync;
	"synced".postln;
	{p = LivePlotter(f, 4096)}.defer;
	y = Synth(\bufWr, [\bus, f.index]);

}
)
s.scope
s.quit
p.scopeView.getProperty(\updateInterval)
p.scopeView.setProperty(\updateInterval, 50)
p.scopeView.setProperty(\yZoom, 0.5)
p.scopeView.setProperty(\yOffset, 0.2)
p.scopeView.yZoom = 1
p.scopeView.yOffset = 0.5

p.scopeView.bounds
p.width / p.scopeView.getProperty(\updateInterval)
p.width / (p.width / p.scopeView.getProperty(\updateInterval)).round(1)
p.scopeView.setProperty(\updateInterval, p.width / (p.width / p.scopeView.getProperty(\updateInterval)).round(1))

s.boot;
b.free; b = Buffer.alloc(s, 800, 2);
b.sine1([1, 1.5], normalize: true, asWavetable: false, clearFirst: true);
q = ScopePlotter.new();
q.buffer_(b)
100.do({|inc| b.set(1129 + inc, 0.9)})
fork{100.do({|inc| b.set(129 + inc, 0.9); 0.01.wait})}
b.set(829, 0.9)
q = b.plot1
q.setRangeY(-1, 3)
q.setGridX(0, b.duration, \s)
q.overlay = false;
q.overlay = true;
q.drawChannelSeparators = true
q.drawYGrid = false;
q.drawXGrid = true;
q.gridViewMain.refresh
b.plot
s.plotTree
q.synth.buffer.plot
fork{300.do({|inc|q.synth.bufPhase_(inc); 0.05.wait})}
(
Routine.run({
	var numSteps = 150;
	var waitTime = 0.05;
	numSteps.do({|inc|
		q.synth.updaterSynth.set(\bufferPhase, (b.numFrames / numSteps) * inc);
		waitTime.wait;
	});
});
)
q.synth.minMax_(-1, 2)
q.synth.bufFramesToRead_(b.numFrames / 2)

c = Buffer.read(s, "/Volumes/data/Dokumenty/2015-2016/Art&Brain research/recording/2016.08.04/00_160804_142639_kr_5minBaseline.WAV");
c.plot1;

q.gridViewChannels[0].background_(Color.rand)

r = ScopePlotter.new(s, c);
r.synth.dump
r.buffer.plot
r.synth.minMax_(22000, 25000)
r.style_(0)

w = Window.new.front;
d = DrawGrid(w);
d = [0, 20].asSpec.grid

FreqScope

DrawGrid

Server.killAll
//class tests
w = Window.new.front;
w.layout = VLayout();
StaticText(w).string_("some text");
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
p.autoRangeY_(true);
p.autoRangeY_(false);

y.free;
p.free;


//see src/supercollider/qtcollider/widgets/qcscopeshm
//for plotting alternating buffers
//see metbhod paint1d