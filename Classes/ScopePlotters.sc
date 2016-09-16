ScopePlotter {
	// copy args;
	var <buffer, <maxBufSize, <name, <bounds, parent;
	
	var makeGui, updateColors;

	var <window, <view;
	var <scopeView;
	var <gridView;
	var <width;

	var <server, <synth;
	var <numChannels;

	var <xSpec, <ySpec, <xGrid, <yGrid;
	var <drawXGrid = true, <drawYGrid = true;

	var <drawChannelSeparators = true, <>channelSeparatorsColor;

	var <>onResize, <>onFree, <>onRun, <>onStop;

	var <overlay = false;

	var <isRunning = false;

	*new {
		arg buffer, maxbufsize = 4096, name, bounds, parent;

		// bus = Bus(rate, index, numChannels, server);

		^super.newCopyArgs(buffer, maxbufsize, name, bounds, parent).init;
	}

	init {
		synth = ScopePlotterSynth(this);

		maxBufSize = max(maxBufSize, 128);
		// Color.hsv(0.6, 1, 0.5).test
		channelSeparatorsColor ?? {channelSeparatorsColor = Color.hsv(0.6, 1, 0.7)}; //blue-ish


		//init
		this.yRange_(-1, 1); //xGrid updated when buffer is loaded

		this.makeGui;
		// this.updateColors;//moved to buffer method

		ServerTree.add(this, server);
		ServerQuit.add(this, server);
		// this.run; //moved to buffer method
		
		buffer !? {
			this.buffer_(buffer)
		}
	}

	buffer_ {arg bufferArg;
		buffer = bufferArg;
		server = buffer.server;
		if(server.isNil) {server = Server.default};
		if(server.isLocal.not) {Error("Can not scope on remote server.").throw};
		scopeView.stop;
		scopeView.server = server;
		scopeView.start;
		numChannels = buffer.numChannels;
		"in buffer_ numChannels: ".post; numChannels.postln;
		this.xGrid_(0, buffer.numFrames);
		this.updateColors;
		this.run; 
	}

	makeGui { // arg parent;

		if( window.notNil ) {window.close};

		if( parent.isNil ) {
			view = window = Window().name_(name);
		}{
			view = View( parent );
			window = nil;
		};
		bounds !? {view.bounds = bounds};

		numChannels ?? {numChannels = 1}; //initially, to avoid errors

		// WIDGETS

		scopeView = ScopeView();
		scopeView.canFocus = true;

		gridView = UserView.new;
		gridView.background = Color.clear;
		gridView.drawFunc = {|thisView|
			var xGridBounds, yGridBounds;
			if(isRunning, {
				xGridBounds = Rect(0, 0, thisView.bounds.width, thisView.bounds.height);
				if(drawXGrid, {
					xGrid.bounds = xGridBounds;
					xGrid.draw;
				});
				if(drawYGrid, {
					if(overlay, {
						yGrid.bounds = xGridBounds;
						yGrid.draw;
					}, {
						var singleChannelHeight;
						singleChannelHeight = thisView.bounds.height/numChannels;
						numChannels.do({|inc|
							yGridBounds = Rect(0, singleChannelHeight * inc, thisView.bounds.width, singleChannelHeight);
							yGrid.bounds = yGridBounds;
							yGrid.draw;
						});
					});
				});
				if(overlay.not && (numChannels > 1) && drawChannelSeparators, { //draw lines between channels
					var singleChannelHeight, thisWidth;
					singleChannelHeight = thisView.bounds.height/numChannels;
					thisWidth = thisView.bounds.width;
					(numChannels - 1).do({|inc|
						var thisY;
						thisY = (inc+1) * singleChannelHeight;
						Pen.fillColor = channelSeparatorsColor;
						Pen.line(0@thisY, thisWidth@thisY);
						Pen.stroke(1);
					})
				})
			})
		};

		view.layout =
		GridLayout()
		.add(scopeView,0,0)
		.add(gridView,0,0)
		.margins_(0).spacing_(0)
		;

		// ACTIONS

		scopeView.onResize_({
			width = scopeView.bounds.width;
			synth.scopeBufSize_(width);
			onResize !? {onResize.(this)};
		});
		view.onClose = { view = nil; this.quit;};

		// LAUNCH

		scopeView.focus;
		width = scopeView.bounds.width; //init
		synth.scopeBufSize_(width); //init
		if( window.notNil ) { window.front };
	}

	updateColors { //hardcoded for now
		var colors;
		if(numChannels == 1, {
			colors = [Color.black];
		}, {
			if(overlay, {
				// colors = numChannels.collect({Color.rand}); 
				colors = numChannels.collect({Color.hsv(rrand(0.0, 1.0), 0.6, 0.5)}); 
			}, {
				colors = numChannels.collect({Color.black});
			});
		});
		scopeView.waveColors = colors;
		scopeView.background = Color.white;
		scopeView.fill = false;
	}

	waveColors_ {arg colors;
		scopeView.waveColors = colors;
	}

	waveColors {^scopeView.waveColors}

	doOnServerTree {
		this.run;
	}

	doOnServerQuit {
		this.stop;
	}

	run {
		if(isRunning, {
			this.stop;
		}); //stop first
		if(buffer.notNil, {
			synth.play(maxBufSize, buffer);
			if( view.notNil && synth.scopeBufferIndex.notNil) {
				scopeView.bufnum = synth.scopeBufferIndex;
				scopeView.start;
			};
			onRun !? {onRun.(this)};
			isRunning = true;
		});
	}

	stop {
		if( view.notNil ) { {scopeView.stop}.defer };
		synth.stop;
		onStop !? {onStop.(this)};
		isRunning = false;
	}

	quit {
		var win;
		this.stop;

		onFree !? {onFree.(this)};
		// "in this.quit, after this.stop".postln;
		synth.free;
		// "after synth free".postln;
		// window.dump;
		if(window.notNil) { win = window; window = nil; {
			try {
				win.close
			} //not sure why this gives an error
		}.defer; };
		// "after window closing".postln;
		ServerTree.remove(this, server);
		ServerQuit.remove(this, server);
		// "end of this.quit".postln;
	}

	free {
		this.quit;
	}

	// updateMinMax {arg min, max;
	// 	//update labels and grids here
	// }
	yRange_ {arg min, max; //this sets vertical range (grid and signal)
		ySpec = [min, max].asSpec;
		yGrid = DrawGrid(nil, nil, ySpec.grid);
		synth.minMax_([min, max]);
		gridView !? {{gridView.refresh}.defer};
	}

	yRange {[ySpec.asSpec.minval, ySpec.asSpec.maxval]} //not sure if this is useful
	
	xGrid_ {arg min, max, units; //this sets horizontal grid only, use this.synth for horizontal sygnal adjustments
		// xSpec = [min, max, units: units].asSpec;
		xSpec = ControlSpec(min, max, units: units);
		xGrid = DrawGrid(nil, xSpec.grid, nil);
		gridView !? {{gridView.refresh}.defer};
	}

	drawXGrid_ {arg val;
		drawXGrid = val.asBoolean;
		gridView !? {{gridView.refresh}.defer};
	}

	drawYGrid_ {arg val;
		drawYGrid = val.asBoolean;
		gridView !? {{gridView.refresh}.defer};
	}

	overlay_ {arg val;
		overlay = val.asBoolean;
		scopeView.style = overlay.asInteger;
		this.updateColors;
		gridView !? {{gridView.refresh}.defer};
	}

	drawChannelSeparators_ {arg val;
		drawChannelSeparators = val.asBoolean;
		gridView !? {{gridView.refresh}.defer};
	}
	
	// keyDown { arg char, mod; //subclasses will implement
	// 	if (mod != 0) { ^false };
	// 	case (
	// 		{ char === $i }, { this.toInputBus },
	// 		{ char === $o }, { this.toOutputBus },
	// 		{ char === $  }, { this.run },
	// 		{ char === $s }, { this.style = (scopeView.style + 1).wrap(0,2) },
	// 		{ char === $S }, { this.style = 2 },
	// 		{ char === $j }, { if(this.index.notNil) {this.index = this.index - 1} },
	// 		{ char === $k }, { this.switchRate; },
	// 		{ char === $l }, { if(this.index.notNil) {this.index = this.index + 1} },
	// 		{ char === $- }, { cycleSlider.increment; cycleSlider.doAction },
	// 		{ char === $+ }, { cycleSlider.decrement; cycleSlider.doAction },
	// 		{ char === $* }, { yZoomSlider.increment; yZoomSlider.doAction },
	// 		{ char === $_ }, { yZoomSlider.decrement; yZoomSlider.doAction },
	// 		{ char === $m }, { this.toggleSize },
	// 		{ char === $.}, { this.stop },
	// 		{ ^false }
	// 	);
	// 	^true;
	// }
}

ScopePlotterSynth {
	// Encapsulate management of server resources
	var plotter; //copyargs; reference ScopePlotter for updating values etc
	var <server, <buffer, <synthDefName, <updaterSynth;
	var scopeUpdateDefName, scopeUpdateSynth;
	var <continuousBuffer, <slidingBuffer, <scopeBuffer;
	var <playThread;
	var updaterArgs, minMax;
	var <scopeBufSize, <bufPhase, <bufStart, <bufFramesToRead;
	var synthDefMain, updaterSynthDef;

	*new { arg plotter;
		var instance;
		// server = server ? Server.default;
		instance = super.newCopyArgs(plotter);
		ServerQuit.add(instance);
		^instance;
	}

	play { arg bufSizeArg, bufferArg;
		var synthArgs;
		var bufIndex;
		var busChannels;
		var numChannels;

		this.stop;

		buffer = bufferArg;

		server = buffer.server;
		if(server.serverRunning.not) { ^this };

		numChannels = buffer.numChannels;

		minMax ?? {minMax = plotter.yRange};

		if (scopeBuffer.isNil) {
			scopeBuffer = ScopeBuffer.alloc(server, numChannels);
			scopeUpdateDefName = "scopeupdate" ++ scopeBuffer.index.asString;
		};

		bufIndex = scopeBuffer.bufnum.asInteger;

		scopeBufSize ?? {scopeBufSize = bufSizeArg};
		bufPhase ?? {bufPhase = 0};
		bufStart ?? {bufStart = 0};
		bufFramesToRead ?? {bufFramesToRead = buffer.numFrames}; //whole buffer by default


		updaterSynthDef = SynthDef(scopeUpdateDefName, { arg scopeBuf = 0, inBuffer = 0, bufSize = 100, minMax = #[-1, 1], bufferPhase = 0/*frames, "rotates" the buffer displayed*/, bufferStart = 0/*frames*/, bufferFramesToRead = 1/*should be specified*/;
			var slidingSignal;
			var phasor, readPhasor;
			var bufMinMax, switchedMinMax, autoMinMaxTrig, minMaxChanged;
			// bufSize = bufSize.min(BufFrames.kr(inBuffer));
			// bufSize.poll;
			phasor = Phasor.ar(1, 1, 0, bufSize.min(bufSizeArg)); //clip to max buffer size to avoid problems
			readPhasor = (((phasor * (bufferFramesToRead / bufSize)) + bufferPhase) % BufFrames.kr(inBuffer)) + bufferStart;
			slidingSignal = BufRd.ar(numChannels, inBuffer, readPhasor);
			slidingSignal = LinLin.ar(slidingSignal, minMax[0], minMax[1], -1, 1); //minmax scaling here
			ScopeOut2.ar(slidingSignal, scopeBuf, bufSizeArg, bufSize); //ar
			// ScopeOut2.kr(slidingSignal, scopeBuf, bufSize, bufSize); //ar
		});
		updaterArgs = [\scopeBuf, scopeBuffer.bufnum, \inBuffer, buffer, \bufSize, scopeBufSize, \minMax, minMax, \bufferFramesToRead, bufFramesToRead.isKindOf(Bus).if({bufFramesToRead.asMap}, {bufFramesToRead}), \bufferPhase, bufPhase.isKindOf(Bus).if({bufPhase.asMap}, {bufPhase}), \bufferStart, bufStart.isKindOf(Bus).if({bufStart.asMap}, {bufStart})];  //NOTE .bufnum is needed for scopeBuffer (!!!) but not for regular buffers
		
		// "plotter.width: ".post; plotter.width.postln;
		playThread = fork {
			// synthDefMain.send(server);
			updaterSynthDef.send(server);
			server.sync;
			// synth = Synth.tail(RootNode(server), synthDef.name, synthArgs);
			// synthMain = Synth.tail(RootNode(server), synthDefMain.name, [\memoryBuf, continuousBuffer, \inBuffer, slidingBuffer, \inbus, bus, \bufSize, plotter.width]); //add rate here! 
			updaterSynth = Synth.tail(RootNode(server), updaterSynthDef.name, updaterArgs);
		}
	}

	stop {
		if (playThread.notNil) { playThread.stop; playThread = nil };
		updaterSynth !? {updaterSynth.free; updaterSynth = nil};
	}

	minMax_ {arg minMaxArg; //should be an array
		minMax = minMaxArg;
		updaterSynth !? { updaterSynth.set(\minMax, minMax) };
	}

	isRunning { ^playThread.notNil }

	scopeBufferIndex { ^ scopeBuffer !? { scopeBuffer.index } } //returns scope buffer...

	// buffer { ^ buffer }

	// setBusIndex { arg index;
	// 	if( synthMain.notNil ) { synthMain.set(\inbus, index) };
	// }

	scopeBufSize_ {arg size;
		scopeBufSize = size;
		// if( synthMain.notNil ) { synthMain.set(\bufSize, size) };
		scopeBuffer !? { //don't run before buffer is allocated
			updaterArgs = [\scopeBuf, scopeBuffer.bufnum, \inBuffer, buffer, \bufSize, scopeBufSize, \minMax, minMax, \bufferFramesToRead, bufFramesToRead.isKindOf(Bus).if({bufFramesToRead.asMap}, {bufFramesToRead}), \bufferPhase, bufPhase.isKindOf(Bus).if({bufPhase.asMap}, {bufPhase}), \bufferStart, bufStart.isKindOf(Bus).if({bufStart.asMap}, {bufStart})];
		};
		if( updaterSynth.notNil ) {
			updaterSynth.free;
			updaterSynth = Synth.tail(RootNode(server), updaterSynthDef.name, updaterArgs); //restarting synth here to make sure the phase of the updater is correct
		};
	}
	
	bufPhase_ { arg phase; //number or bus
		bufPhase = phase;
		if( updaterSynth.notNil ) {	updaterSynth.set(\bufferPhase, bufPhase.isKindOf(Bus).if({bufPhase.asMap}, {bufPhase}))};
	}
	bufStart_ {arg start; //number or bus
		bufStart = start;
		if( updaterSynth.notNil ) { updaterSynth.set(\bufferStart, bufStart.isKindOf(Bus).if({bufStart.asMap}, {bufStart})) };
	}
	bufFramesToRead_ {arg frames; //number or bus
		bufFramesToRead = frames;
		if( updaterSynth.notNil ) { updaterSynth.set(\bufferFramesToRead, bufFramesToRead.isKindOf(Bus).if({bufFramesToRead.asMap}, {bufFramesToRead})) };
	}
	
	free {
		this.stop;
		[scopeBuffer].do({|thisOne|
			if (thisOne.notNil) {
				thisOne.free;
				thisOne = nil;
			};
		});
		ServerQuit.remove(this, server);
	}

	doOnServerQuit {
		scopeBuffer = nil;
		updaterSynth = nil;
	}
}

LivePlotter : ScopePlotter {
	var <bus;

	var <rate = 100;

	var <autoYRange;
	var <synthLive;
	var <>currentBufferMinMax;

	*new {arg bus, bufsize = 4096, name, bounds, parent;
		^super.newCopyArgs(nil, bufsize, name, bounds, parent).init.initLP(bus);
	}

	initLP {arg busArg;
		bus = busArg;
		"bus: ".post; bus.postln;
		"buffer: ".post; buffer.postln;

		synthLive = LivePlotterSynth(this);

		//following functions are called by superclass on appropriate actions
		onRun = {synthLive.play(bus)}; 
		onStop = {synthLive.stop};
		onFree = {synthLive.free};
		onResize = {this.updateRateAndXGrid};
		
		bus !? {
			this.bus_(bus);
		}
	}
	
	bus_ {arg busArg;
		bus = busArg;
		synthLive.initBuffer(bus.server, maxBufSize, bus.numChannels);
		this.buffer_(synthLive.buffer);
		this.rate_(rate); //update
	}

	rate_ {arg rateArg; //in pixels / second!
		rate = rateArg;
		this.updateRateAndXGrid; //this also adjusts rate
	}

	updateRateAndXGrid { //private
		synthLive.rate_(rate * (maxBufSize / width)); //rescale so it's always pixels/s
		this.xGrid_(width / rate.neg, 0, \s);
	}

	autoYRange_ {arg val;
		autoYRange = val.asBoolean;
		this.synthLive.autoMinMax_(val);
	}
}


LivePlotterSynth {
	// Encapsulate management of server resources

	var <server, <buffer, <updaterSynth, <minMaxSynth;
	var <continuousBuffer, <slidingBuffer, <scopeBuffer;
	var <recSynthDefName, <autoMinMaxDefName;
	var <playThread;
	var rangeResp, resizeResp;
	var resizeReplyName, rangeReplyName;
	var livePlotter; //reference ScopeLivePlotter for updating values etc
	var <autoMinMax, <minMax, <synthRefreshUpdateRate, <minMaxLagTime, updaterArgs;
	var phasorBus;
	var minMaxSynthDef, autoMinMaxSynthDef;
	var recSynthDef;
	var <rate = 1;
	var numChannels;

	*new { arg livePlotter;
		var instance;
		// server = server ? Server.default;
		instance = super.new.init(livePlotter);
		ServerQuit.add(instance);
		^instance;
	}

	init {arg livePlotter_;
		livePlotter = livePlotter_;
		autoMinMax = false;
		synthRefreshUpdateRate = 20;
		minMaxLagTime = 0.4;
	}

	initBuffer {arg serverArg, bufSizeArg, numChannelsArg;
		server = serverArg;
		numChannels = numChannelsArg;
		buffer !? {buffer.free};
		buffer = Buffer.alloc(server, bufSizeArg, numChannels);
	}

	play { arg bus;
		var synthArgs;
		var bufIndex;
		// var busChannels;
		var bufSizeArg;

		bufSizeArg = buffer.numFrames;

		"numChannels: ".post; numChannels.postln;

		if(server.serverRunning.not) { ^this };

		this.stop;

		// if (scopeBuffer.isNil) {
		// scopeBuffer = ScopeBuffer.alloc(server, numChannels);
		recSynthDefName = "scopeRec" ++ buffer.bufnum.asString;
		autoMinMaxDefName = "scopeAutoMinMax" ++ buffer.bufnum.asString;
		resizeReplyName = ("/" ++ recSynthDefName ++ "_resize").asSymbol;
		rangeReplyName = ("/" ++ recSynthDefName ++ "_range").asSymbol;
		// };
		phasorBus ?? {phasorBus = Bus.control(server, 1)};

		//responders

		rangeResp !? {rangeResp.free};
		rangeResp ?? {
			rangeResp = OSCFunc({|msg|
				// msg.postln;
				// msg[3..].postln;
				livePlotter.currentBufferMinMax = msg[[3,4]];
				if(autoMinMax, {livePlotter.yRange_(msg[3], msg[4])});
			}, rangeReplyName);
		};

		//synthdefs
		recSynthDef = SynthDef(recSynthDefName, { arg buf = 0, inbus = 0, phaseout = 0, rate = 100, phasorOutOffset = 0/*for aligning plot precisely... temp*/; //updateRate is for various things
			var inSig, memPhasor, slidingSig, slidingPhasor, scopeRdPhasor, memPhasorLatched;
			var bufSizeChanged;
			var updateTrig;
			var bufSize;
			// var memPhasor1;

			inSig = In.kr(inbus, numChannels); //figure how to pass an array here maybe
			bufSize = BufFrames.kr(buf);
			// memPhasor1 = Phasor.kr(1, rate * ControlDur.ir, 0, bufSize); //rate scaled so it's samples/second
			memPhasor = Phasor.ar(1, rate * SampleDur.ir, 0, bufSize); //ar to address issue of skipping samples; rate scaled so it's samples/second
			// BufWr.kr(inSig, buf, memPhasor); //storing signal of the last number of samples
			BufWr.ar(K2A.ar(inSig), buf, memPhasor); //storing signal of the last number of samples

			// Out.kr(phaseout, memPhasor);
			// Out.kr(phaseout, A2K.kr(memPhasor));
			Out.kr(phaseout, A2K.kr(memPhasor) + phasorOutOffset);
		});

		autoMinMaxSynthDef = SynthDef(autoMinMaxDefName, { arg buf = 0, minMaxLagTime = 0.2, updateRate = 20, restartRefreshingTime = 1, minMinMaxDifference = 0.1;
			var slidingSignal;
			var bufMinMax, switchedMinMax, autoMinMaxTrig, minMaxChanged;

			//for auto min max
			autoMinMaxTrig = Impulse.kr(updateRate);
			bufMinMax = [BufMin.kr(buf, autoMinMaxTrig)[0], BufMax.kr(buf, autoMinMaxTrig)[0]];
			// bufMinMax.poll;
			// switchedMinMax = Select.kr(autoMinMax, [minMax, bufMinMax]);
			switchedMinMax = bufMinMax;

			switchedMinMax[1] = switchedMinMax[1].max(switchedMinMax[0] + minMinMaxDifference);
			switchedMinMax = Lag.kr(switchedMinMax, minMaxLagTime);
			minMaxChanged = EnvGen.kr(Env([0, 1, 0], [0, restartRefreshingTime]), Changed.kr(switchedMinMax).sum).roundUp;
			// minMaxChanged.poll;
			SendReply.kr(autoMinMaxTrig * minMaxChanged, rangeReplyName, switchedMinMax);
		});
		updaterArgs = [\buf, buffer, \inbus, bus, \phaseout, phasorBus, \rate, rate]; 
		// "livePlotter.width: ".post; livePlotter.width.postln;
		playThread = fork {
			recSynthDef.send(server);
			autoMinMaxSynthDef.send(server);
			server.sync;
			updaterSynth = Synth.tail(RootNode(server), recSynthDef.name, updaterArgs);
			minMaxSynth = Synth.tail(RootNode(server), autoMinMaxSynthDef.name, [\buf, buffer, \updateRate, synthRefreshUpdateRate, \minMaxLagTime, minMaxLagTime]); 
		};
		//important, set the phase advancing
		livePlotter.synth.bufPhase_(phasorBus);
	}

	stop {
		if (playThread.notNil) { playThread.stop; playThread = nil };
		updaterSynth !? {updaterSynth.free; updaterSynth = nil};
		minMaxSynth !? {minMaxSynth.free; minMaxSynth = nil};
	}

	autoMinMax_ {arg val;
		autoMinMax = val.asBoolean;
	}


	synthRefreshUpdateRate_ {arg rateArg;
		synthRefreshUpdateRate = rateArg;
		// synthMain !? { synthMain.set(\updateRate, rateArg) };
		minMaxSynth !? { minMaxSynth.set(\updateRate, synthRefreshUpdateRate) };
	}

	minMaxLagTime_ {arg time;
		minMaxLagTime = time;
		// synthMain !? { synthMain.set(\minMaxLagTime, time) };
		minMaxSynth !? { minMaxSynth.set(\minMaxLagTime, minMaxLagTime) };
	}

	isRunning { ^playThread.notNil }

	bufferIndex { ^ scopeBuffer !? { scopeBuffer.index } } //but we have many buffers

	bus_ { arg busArg;
		if( updaterSynth.notNil ) { updaterSynth.set(\inbus, busArg) };
	}

	rate_ {arg rateArg; //rate here expressed in samples per second
		rate = rateArg;
		if( updaterSynth.notNil ) { updaterSynth.set(\rate, rateArg) };
	}
	
	free {
		this.stop;
		[buffer, rangeResp].do({|thisOne|
			if (thisOne.notNil) {
				thisOne.free;
				thisOne = nil;
			};
		});
		ServerQuit.remove(this, server);
	}

	doOnServerQuit {
		// buffer = nil;
		// synth = nil;
		scopeBuffer = nil;
		continuousBuffer = nil;
		slidingBuffer = nil;
		minMaxSynth = nil;
		updaterSynth = nil;
	}
}

+ Buffer {
	plot0 { |name, bounds, minval, maxval, separately = false| //temporary basic plotter
		var plotter;
		plotter = ScopePlotter(this.server, this, 4096, name, bounds);
		if((minval.notNil && maxval.notNil), {plotter.setRangeY_(minval, maxval)});
		^plotter;
	}
}