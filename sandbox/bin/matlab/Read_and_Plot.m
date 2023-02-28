close all
clear all
clc


fncinfo = ncinfo('NESBA_SBC21_25m_bathy_Oct20.nc');

fncinfo.Variables.Name;

x = ncread('NESBA_SBC21_25m_bathy_Oct20.nc','x');
y = ncread('NESBA_SBC21_25m_bathy_Oct20.nc','y');
t = ncread('NESBA_SBC21_25m_bathy_Oct20.nc','tif');
m = ncread('NESBA_SBC21_25m_bathy_Oct20.nc','transverse_mercator');

figure;pcolor(x,y,t')
shading flat
caxis([-4000 00])
caxis([-400 -200])
ylim([4.424 4.434]*1e6)
xlim([3.3 3.5]*1e5)


iy =find(y<=4.429*1e6,1)
figure;plot((x-3.3*1e5)/1e3,t(:,iy))
xlim(([3.3 3.5]*1e5-3.3*1e5)/1e3)
box on
grid on
xlabel('Range [km]')
ylabel('Depth [m]')

[xx yy]=meshgrid(x,y);
for ii=1:size(xx,1)   
    [Lat(ii,:),Lon(ii,:)]=utm2wgs(xx(ii,:)',yy(ii,:)',repmat(['19T']',1,size(xx(ii,:),2))');
end

% bathy
figure;pcolor(Lon,Lat,t')
shading flat
% caxis([-4000 00])
caxis([-400 -200])
axis equal
ylim([39.95 40.05])
xlim([-71 -70.8])
colorbar('NorthOutside')

% residual bathy
figure;pcolor(Lon,Lat,t-tn)
shading flat
% caxis([-4000 00])
caxis([-1 1]*10)
axis equal
ylim([39.95 40.05])
xlim([-71 -70.8])
colorbar('NorthOutside')


iy = find(Lat(:,3580)<=40,1);
ix1 = find(Lon(1,:)>=-71,1);
ix2 = find(Lon(1,:)>=-70.8,1);

figure;plot((xx(1,ix1:ix2)-xx(1,ix1))/1e3,t(ix1:ix2,iy))
box on
grid on
xlabel('Range [km]')
ylabel('Depth [m]')
xlim([0 inf])


hold on
h1 = fill([-71.166 -71.166 -70.501 -70.501],[40.066 39.95 39.92 40.056 ],'r');
h1.FaceAlpha = .0;
% save Bathy Lon Lat t xx yy

return
%%


win = 50;
tn = t;
for ii=1:6337-win
    for jj=3500:4500
        tn(ii+win/2,jj+win/2) = mean(mean(t(ii-1+(1:win),jj-1+(1:win))));
    end
    ii
end

figure;
subplot(211);pcolor(Lon(1:6337,3500:4500),Lat(1:6337,3500:4500),-t(1:6337,3500:4500))
shading flat
caxis([0 2000])
 ylim([39.8 40])
xlim([-71.25 -70])

subplot(212);pcolor(Lon(1:6337,3500:4500),Lat(1:6337,3500:4500),-t(1:6337,3500:4500)+tn(1:6337,3500:4500))
shading flat
caxis([-1 1]*10)
 ylim([39.8 40])
xlim([-71.25 -70])


return


dx = diff(t)./diff(xx');
dy = (diff(t')./diff(yy))';
BathyGrad = dx(:,1:end-1)+dy(1:end-1,:);

figure;pcolor(Lon(1:end-1,1:end-1),Lat(1:end-1,1:end-1),abs(BathyGrad'))
shading flat
% caxis([-4000 00])
% caxis([-400 -200])
% caxis([-300 -250])
axis equal
ylim([39.95 40.05])
xlim([-71 -70.8])
colorbar('NorthOutside')
caxis([0 1]*.1)
colormap gray
