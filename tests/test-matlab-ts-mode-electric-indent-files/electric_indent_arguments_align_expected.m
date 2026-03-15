% -*- matlab-ts -*-

function electric_indent_arguments_align(reallyLongLongLongLongLongLongLongLongInput1, in2, opts)
    arguments (Input)
        reallyLongLongLongLongLongLongLongLongInput1 (1,2) {mustBeFinite}
        in2                                          {mustBeA(in1, {'single', 'double'}), mustBeReal, mustBeFinite}
        opts.AddOperationalAuditoryContributions     (1,1) logical {} = true
        opts.fooSignalLevel                          (1,5) {mustBeA(opts.fooSignalLevel, {'single', 'double'}), mustBeReal}
        opts.fooNoiseLevel                           (1,5) {mustBeA(opts.fooNoiseLevel, {'single', 'double'}), mustBeReal}
        opts.OperationalSignalLevel                  (1,5) {mustBeA(opts.OperationalSignalLevel, {'single', 'double'}), mustBeReal} = 60 + [-2.5, 1.5, 0, -6.2, -12];
        opts.OperationalNoiseLevel                   (1,5) {mustBeA(opts.OperationalNoiseLevel, {'single', 'double'}), mustBeReal} = [40, 36, 34, 33, 32];
        opts.AddOperationalAmbientNoise              (1,1) logical {} = true
        opts.fooBar                                  logical {} = true
        opts.onOff                                   (1,1) matlab.lang.OnOffSwitchState = 'on'
    end
    disp(reallyLongLongLongLongLongLongLongLongInput1);
    disp(in2);
    disp(opts);
end
