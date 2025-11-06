% -*- matlab-ts -*-

% Structure, bar and disp are valid fields and shouldn't have builtin face.
foo.bar.disp.goo = 1;

%  Case: "Simulink.MDLInfo.FileName" 'property
x = Simulink.MDLInfo.FileName;

%  Case: "Simulink.CMI.CompiledSimType.ModelApi" 'enumeration
y = Simulink.CMI.CompiledSimType.ModelApi;

% genapp is a function in the simulink.compiler namespace
simulink.compiler.genapp('modelName', foo)

% test with no args (though this will error when run)
simulink.compiler.genapp()

% test again with no args (though this will error when run)
simulink.compiler.genapp
