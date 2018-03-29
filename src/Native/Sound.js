var _illi_ichi$illi_ichi$Native_Sound = (function(){
    var AudioContext = window.webkitAudioContext||window.AudioContext;
    var audioctx = new AudioContext();

    function currentTime(){
        return audioctx.currentTime;
    }

    function play(currentTime, freq, long, after){
        var osc = audioctx.createOscillator();
        var gain = audioctx.createGain();

        var attack = 0.05;
        var decay = 0.2;
        var t0 = currentTime + after;

        osc.type = 'sine';
        osc.connect(gain);
        gain.connect(audioctx.destination);

        osc.frequency.value = freq;
        osc.detune.value = 50;
        gain.gain.cancelScheduledValues(t0);
        gain.gain.setValueAtTime(0, t0);
        gain.gain.linearRampToValueAtTime(0.5, t0 + attack);
        gain.gain.exponentialRampToValueAtTime(0.000001, t0 + long);

        osc.start(t0);
        osc.stop(t0 + long);
    }

    return {
        currentTime: currentTime,
        play: F4(play)
    };
})();
