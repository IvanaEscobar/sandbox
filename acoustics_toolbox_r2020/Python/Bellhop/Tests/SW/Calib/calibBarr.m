clear all, close all

addpath /home/orodrig/FORdoc/at/Matlab/ReadWrite

disp('Bellhop - calibration:')
disp('arrival calculation') 

system('bellhop.exe calibBarr');

[Arr, Pos] = read_arrivals_asc( 'calibBarr.arr', 18 );

A         = Arr.A;
delay     = Arr.delay;
Narr      = Arr.Narr; 
NumTopBnc = Arr.NumTopBnc;
NumBotBnc = Arr.NumBotBnc;
RcvrAngle = Arr.RcvrAngle;
SrcAngle  = Arr.SrcAngle;

zs     = Pos.s.depth;
rarray = Pos.r.range;
zarray = Pos.r.depth;
    
Nsd = length( zs );
Nrd = length( zarray );
Nrr = length( rarray );

figure(1), hold on
for i = 2:Nrr

tau =  delay(i,1:Narr(i)); 
amp = abs( A(i,1:Narr(i)) );
rangei = rarray(i)*ones(1,Narr(i));
stem3(tau,rangei,amp)
end 
box on, grid on
hold off
view(22,30)
xlabel('Travel time (s)')
ylabel('Range (in m)')
zlabel('Amplitude')
title('Bellhop - calibration')

disp('done.')
