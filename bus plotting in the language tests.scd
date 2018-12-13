
(
~plot = {|inbus = 0, parent, plotSize = 5, updateRate = 20| //only single channel for now
	var arr, phase = 0, updateRateReci, width, height, view, window, busIndex, numChannels, updateRoutine, refreshRoutine, pointTimeReciprocal, updateVals;
	inbus.postln;
	if(inbus.isKindOf(Bus).not, {
		inbus = Bus.new(\control, inbus, 1, Server.default);
	});
	busIndex = inbus.asUGenInput;
	numChannels = inbus.numChannels;
	if(parent.notNil, {
		view = UserView(parent);
	}, {
		window = Window.new("plot " ++ busIndex, 300@200).front;
		view = UserView(window, window.view.bounds);
	});

	view.background_(Color.white);
	view.resize = 5;

	arr = [0];

	updateRateReci = updateRate.reciprocal;

	updateVals = {|v|
		width = v.bounds.width;
		height = v.bounds.height;
		pointTimeReciprocal = (plotSize / width);
		"pointTimeReciprocal: ".post; pointTimeReciprocal.postln;
		phase = phase.min(width);
		arr = arr.resamp0(width);
		// width.postln;
		// arr.postln;
	};

	updateVals.(view);

	view.onResize_(updateVals);

	updateRoutine = Routine.run({
		loop{
			arr[phase.min(arr.size)] = inbus.getSynchronous;
			phase = (phase + 1) % width;
			pointTimeReciprocal.wait;
		}
	});
	view.drawFunc = {
		Pen.strokeColor = Color.black;
		Pen.moveTo(0@height/2);
		width.do({ |inc|
			// var value = arr[(phase + inc) % width];
			var value = arr[(phase + inc) % arr.size];
			Pen.lineTo(inc@value.linlin(-1, 1,  height, 0));
		});
		Pen.stroke;
	};
	refreshRoutine = Routine.run({
		loop{
			view.refresh;
			updateRateReci.wait;
		}
	}, clock: AppClock);
	view.onClose_({
		updateRoutine.stop;
		refreshRoutine.stop;
	});
};
)
~plot.(b)
32.do({~plot.(b, w)})
~plot.(b, w)

LivePlotter(b)
32.do({LivePlotter(b,parent: w)})



(
s.waitForBoot({
	b = Bus.control;
	// SynthDef(\buswr, {arg bus; Out.kr(b.index, MouseY.kr(-2, 2))}).add;
	SynthDef(\buswr, {arg bus; Out.kr(b.index, WhiteNoise.kr)}).add;
	s.sync;
	// 0.1.wait;
	y.free; y = Synth(\buswr);
	y.free;
});
)

b.scope

w = Window.new.front
w.layout = VLayout()







//kr plotting textx

(
s.boot;
r.stop;
b = Bus.control(s, 1);
w = Window().front;
v = w.view;
v.background = Color.clear();
v.onResize_({|view|
	"I am running ".post; view.class.postln;
})
~arr = 0!v.bounds.width;
~phase = 0;
~updateRate = 20; //Hz
~plotSize = 5; //seconds
~pointTime =~plotSize / v.bounds.width;
~udpateRate =
~width = v.bounds.width;
~height =  v.bounds.height;
r = Routine.run({
	loop{
		~arr[~phase] = b.getSynchronous;
		~phase = (~phase + 1) % ~width;
		~updateRate.reciprocal.wait;
	}
});
// v.asView
w.drawFunc = {
	 Pen.strokeColor = Color.black;
	Pen.moveTo(0@~height/2);
    ~arr.size.do { |inc|
		var value = ~arr[(~phase + inc) % ~width];
		// var value = ~arr[inc];
		Pen.lineTo(inc@value.linlin(-1, 1,  ~height, 0));
			// Pen.translate(10, 0); // shift 10 to the right every time
    };
	Pen.stroke
};
u = Routine.run({
	loop{
		w.refresh;
		~updateRate.reciprocal.wait;
		// 0.1.wait;
	}
}, clock: AppClock);

r.stop;
u.stop;

SynthDef(\buswr, {arg bus; Out.kr(b.index, MouseY.kr(-2, 2))}).add;
s.sync;
y.free; y = Synth(\buswr);
y.free;
)

onResize
resize
r.stop;
u.stop;

SynthDef(\buswr, {arg bus; Out.kr(b.index, MouseY.kr(-2, 2))}).add;
s.sync;
y.free; y = Synth(\buswr);
y.free;
)
b.scope
(
w = Window.new.front;
w.view.background_(Color.clear);
w.drawFunc = {
    Pen.strokeColor = Color.black;
    35.do { // draw 35 lines
        Pen.moveTo(0@0);
        Pen.lineTo(50@350);
        Pen.translate(10, 0); // shift 10 to the right every time
        Pen.stroke
    }
};
w.refresh;
)

b.scope
b.index
RunningMax
~arr.plot