%inspection, face1, for this face, North obcs: valid up to index 1424 in x-dir
i=1;
ix=1563:ncut1;iy= 271:ncut2;bathy{i}(ix,iy)=0; ix=1425:ncut1;iy=1058:ncut2;bathy{i}(ix,iy)=0;
ix=1425:ncut1;iy=1045:ncut2;bathy{i}(ix,iy)=0; ix=1421:1423; iy=1051:1054; bathy{i}(ix,iy)=0;
ix=1383:1385; iy=1034:1037; bathy{i}(ix,iy)=0; ix=1392:1395; iy=1043:1047; bathy{i}(ix,iy)=0;
ix=1378:1387; iy=1019:1026; bathy{i}(ix,iy)=0; ix=1385:1398; iy=1010:1018; bathy{i}(ix,iy)=0;
ix=1395:1408; iy= 989:1005; bathy{i}(ix,iy)=0; ix=1383:1385; iy=1016:1019; bathy{i}(ix,iy)=0;
ix=1388:1394; iy=1007:1010; bathy{i}(ix,iy)=0; ix=1391:1396; iy= 993: 998; bathy{i}(ix,iy)=0;
ix=1396:1398; iy= 971: 989; bathy{i}(ix,iy)=0; ix=1391:1397; iy= 983: 993; bathy{i}(ix,iy)=0;
ix=1398:1416; iy= 963: 988; bathy{i}(ix,iy)=0; ix=1397:1405; iy= 934: 952; bathy{i}(ix,iy)=0;
ix=1402:1404; iy= 915: 929; bathy{i}(ix,iy)=0; ix=1401:1403; iy= 925: 930; bathy{i}(ix,iy)=0;
ix=1398:1404; iy= 931: 935; bathy{i}(ix,iy)=0; ix=1405:1413; iy= 860: 914; bathy{i}(ix,iy)=0;
ix=1399:1412; iy= 822: 835; bathy{i}(ix,iy)=0; ix=1388:1393; iy= 789: 794; bathy{i}(ix,iy)=0;
ix=1371:1374; iy= 754: 757; bathy{i}(ix,iy)=0; ix=1383:1401; iy= 725: 758; bathy{i}(ix,iy)=0;
ix=1377:1384; iy= 731: 738; bathy{i}(ix,iy)=0; ix=1394:1411; iy= 717: 726; bathy{i}(ix,iy)=0;
ix=1403:1411; iy= 700: 717; bathy{i}(ix,iy)=0; ix=1401:1411; iy= 713: 717; bathy{i}(ix,iy)=0;
ix=1410:1417; iy= 630: 634; bathy{i}(ix,iy)=0; ix=1468:1472; iy= 632: 639; bathy{i}(ix,iy)=0;
ix=1480:1495; iy= 634: 641; bathy{i}(ix,iy)=0; ix=1491:1495; iy= 632: 641; bathy{i}(ix,iy)=0;
ix=1515:1537; iy= 614: 638; bathy{i}(ix,iy)=0; ix=1392:1396; iy= 621: 626; bathy{i}(ix,iy)=0;
ix=1398:1401; iy= 624: 627; bathy{i}(ix,iy)=0; ix=1518:1537; iy= 599: 638; bathy{i}(ix,iy)=0;
ix=1524:1533; iy= 587: 596; bathy{i}(ix,iy)=0; ix=1531:1541; iy= 568: 583; bathy{i}(ix,iy)=0;
ix=1531:1536; iy= 509: 521; bathy{i}(ix,iy)=0; ix=1530:1535; iy= 499: 506; bathy{i}(ix,iy)=0;
ix=1523:1526; iy= 479: 488; bathy{i}(ix,iy)=0; ix=1522:1526; iy= 484: 490; bathy{i}(ix,iy)=0;
ix=1401:1403; iy= 362: 364; bathy{i}(ix,iy)=0; ix=1353:1356; iy= 173: 177; bathy{i}(ix,iy)=0;
ix=1350:1354; iy= 168: 173; bathy{i}(ix,iy)=0; ix=1345:1350; iy= 162: 168; bathy{i}(ix,iy)=0;
ix=1327:1335; iy= 134: 141; bathy{i}(ix,iy)=0; ix=1320:1327; iy= 124: 132; bathy{i}(ix,iy)=0;
ix=1315:1318; iy= 120: 125; bathy{i}(ix,iy)=0; ix=1244:1256; iy=  66:  74; bathy{i}(ix,iy)=0;
ix=1221:1228; iy=  62:  66; bathy{i}(ix,iy)=0; ix=1202:1239; iy=  39:  55; bathy{i}(ix,iy)=0;
ix=1228:1239; iy=  57:  66; bathy{i}(ix,iy)=0; ix=1182:1191; iy=  20:  38; bathy{i}(ix,iy)=0;

%face5: imask for West: starts on [24,609], so anything below i=26 and j=609 can be zeroed out
%1st wt pt: [25,609], 2nd wt pt: [26,609]
i=5;
ix=   1:  25;iy=   1: 608;bathy{i}(ix,iy)=0; %THis line is valid
ix=  23: 170;iy=  94: 516;bathy{i}(ix,iy)=0;
ix=  44:  51;iy= 576: 589;bathy{i}(ix,iy)=0; ix=  88:  89;iy= 581: 583;bathy{i}(ix,iy)=0;
ix=  99: 107;iy= 573: 579;bathy{i}(ix,iy)=0; ix=95;iy=580;bathy{i}(ix,iy)=bathy{i}(ix,iy-1);
ix= 103: 107;iy= 580: 581;bathy{i}(ix,iy)=0;
ix= 121: 129;iy= 589: 594;bathy{i}(ix,iy)=0; ix= 126: 129;iy= 595: 595;bathy{i}(ix,iy)=0;
ix= 136: 158;iy= 554: 569;bathy{i}(ix,iy)=0; ix= 159: 161;iy= 553: 568;bathy{i}(ix,iy)=0;
ix= 163: 166;iy= 550: 553;bathy{i}(ix,iy)=0; ix= 142: 146;iy= 602: 626;bathy{i}(ix,iy)=0;
ix= 131: 139;iy= 624: 625;bathy{i}(ix,iy)=0; ix= 150: 153;iy= 602: 604;bathy{i}(ix,iy)=0;
ix= 152: 154;iy= 598: 600;bathy{i}(ix,iy)=0; ix= 171: 173;iy= 510: 514;bathy{i}(ix,iy)=0;
ix= 177: 181;iy= 473: 476;bathy{i}(ix,iy)=0; ix= 172: 221;iy= 417: 440;bathy{i}(ix,iy)=0;
ix= 139: 140;iy= 621: 624;bathy{i}(ix,iy)=0; ix= 149: 150;iy= 610: 612;bathy{i}(ix,iy)=0;
ix= 141: 144;iy= 587: 595;bathy{i}(ix,iy)=0; ix= 148: 150;iy= 579: 585;bathy{i}(ix,iy)=0;
ix= 194: 200;iy= 499: 511;bathy{i}(ix,iy)=0; ix= 192: 193;iy= 507: 515;bathy{i}(ix,iy)=0;
ix= 188: 193;iy= 507: 515;bathy{i}(ix,iy)=0; ix= 202: 209;iy= 438: 451;bathy{i}(ix,iy)=0;
ix= 199: 204;iy= 453: 465;bathy{i}(ix,iy)=0; ix= 215: 220;iy= 437: 458;bathy{i}(ix,iy)=0;
ix= 222: 244;iy= 412: 432;bathy{i}(ix,iy)=0; ix= 258: 279;iy= 421: 427;bathy{i}(ix,iy)=0;
ix= 280: 284;iy= 420: 424;bathy{i}(ix,iy)=0; ix= 285: 289;iy= 419: 422;bathy{i}(ix,iy)=0;
ix= 290: 293;iy= 405: 420;bathy{i}(ix,iy)=0; ix= 294: 302;iy= 405: 415;bathy{i}(ix,iy)=0;
ix= 303: 307;iy= 354: 410;bathy{i}(ix,iy)=0; ix= 288: 296;iy= 412: 418;bathy{i}(ix,iy)=0;
ix= 306: 311;iy= 394: 404;bathy{i}(ix,iy)=0; ix= 308: 312;iy= 372: 389;bathy{i}(ix,iy)=0;
ix= 313: 319;iy= 376: 399;bathy{i}(ix,iy)=0; ix= 308: 335;iy= 330: 365;bathy{i}(ix,iy)=0;
ix= 283: 406;iy= 267: 359;bathy{i}(ix,iy)=0; ix= 255: 306;iy= 353: 390;bathy{i}(ix,iy)=0;
ix= 406: 481;iy= 267: 337;bathy{i}(ix,iy)=0; ix= 407: 435;iy= 335: 350;bathy{i}(ix,iy)=0;
ix= 416: 429;iy= 350: 354;bathy{i}(ix,iy)=0; ix= 431: 433;iy= 350: 352;bathy{i}(ix,iy)=0;
ix= 436: 440;iy= 338: 348;bathy{i}(ix,iy)=0; ix= 441: 443;iy= 338: 346;bathy{i}(ix,iy)=0;
ix= 444: 448;iy= 338: 344;bathy{i}(ix,iy)=0; ix= 449: 451;iy= 336: 342;bathy{i}(ix,iy)=0;
ix= 376: 378;iy= 371: 377;bathy{i}(ix,iy)=0; ix= 379: 381;iy= 370: 376;bathy{i}(ix,iy)=0;
ix= 382: 388;iy= 365: 374;bathy{i}(ix,iy)=0; ix= 389: 394;iy= 362: 371;bathy{i}(ix,iy)=0;
ix= 396: 403;iy= 358: 367;bathy{i}(ix,iy)=0; ix= 417: 424;iy= 350: 354;bathy{i}(ix,iy)=0;
ix= 425: 432;iy= 350: 353;bathy{i}(ix,iy)=0; ix= 436: 438;iy= 338: 349;bathy{i}(ix,iy)=0;
ix= 439: 441;iy= 338: 347;bathy{i}(ix,iy)=0; ix= 442: 447;iy= 338: 345;bathy{i}(ix,iy)=0;
ix= 448: 452;iy= 337: 343;bathy{i}(ix,iy)=0; ix= 453: 458;iy= 338: 340;bathy{i}(ix,iy)=0;
ix= 313: 315;iy= 399: 401;bathy{i}(ix,iy)=0; ix= 320: 323;iy= 387: 397;bathy{i}(ix,iy)=0;
ix= 324: 325;iy= 392: 395;bathy{i}(ix,iy)=0; ix= 328: 332;iy= 387: 391;bathy{i}(ix,iy)=0;
ix= 331: 339;iy= 366: 370;bathy{i}(ix,iy)=0; ix= 340: 344;iy= 372: 376;bathy{i}(ix,iy)=0;
ix= 348: 372;iy= 371: 379;bathy{i}(ix,iy)=0; ix= 395: 399;iy= 355: 369;bathy{i}(ix,iy)=0;
ix= 477: 555;iy= 280: 338;bathy{i}(ix,iy)=0; ix= 484: 531;iy= 339: 341;bathy{i}(ix,iy)=0;
ix= 501: 535;iy= 342: 346;bathy{i}(ix,iy)=0; ix= 494: 501;iy= 341: 343;bathy{i}(ix,iy)=0;
ix= 513: 550;iy= 347: 348;bathy{i}(ix,iy)=0; ix= 521: 541;iy= 349: 351;bathy{i}(ix,iy)=0;
ix= 525: 537;iy= 352: 352;bathy{i}(ix,iy)=0; ix= 527: 537;iy= 353: 353;bathy{i}(ix,iy)=0;
ix= 529: 537;iy= 354: 354;bathy{i}(ix,iy)=0; ix= 531: 537;iy= 355: 355;bathy{i}(ix,iy)=0;
ix=533:598;iy=280:355;
tmp=bathy{i};tmp1=smooth2a(tmp,2);
for j=1:length(iy);
  ik=find(tmp(ix,iy(j))<10);
  ij=find(diff(tmp1(ix(1:ik(end)),iy(j)))<0);
  if(length(ij)>0);ii=1:ij(end)+1;tmp(ix(ii),iy(j))=0;end;
end;
bathy{i}=tmp;
ix= 533: 573;iy= 356;     bathy{i}(ix,iy)=0; ix= 535: 573;iy=357;      bathy{i}(ix,iy)=0;
ix= 539: 573;iy= 358;     bathy{i}(ix,iy)=0; ix= 542: 572;iy=359;      bathy{i}(ix,iy)=0;
ix= 546: 565;iy= 360;     bathy{i}(ix,iy)=0; ix= 547: 556;iy=361;      bathy{i}(ix,iy)=0;
ix= 596: 600;iy= 314: 316;bathy{i}(ix,iy)=0; ix= 594: 606;iy= 273: 311;bathy{i}(ix,iy)=0;
ix= 603: 618;iy= 266: 272;bathy{i}(ix,iy)=0; ix= 610: 612;iy= 278: 280;bathy{i}(ix,iy)=0;
ix= 622: 639;iy= 232: 248;bathy{i}(ix,iy)=0; ix= 640: 652;iy= 232: 244;bathy{i}(ix,iy)=0;
ix= 647: 657;iy= 197: 242;bathy{i}(ix,iy)=0; ix= 658: 661;iy= 202: 211;bathy{i}(ix,iy)=0;
ix= 664: 667;iy= 197: 201;bathy{i}(ix,iy)=0; ix= 682: 696;iy= 140: 184;bathy{i}(ix,iy)=0;
ix= 697: 704;iy= 169: 182;bathy{i}(ix,iy)=0; ix= 705: 707;iy= 140: 181;bathy{i}(ix,iy)=0;
ix= 708: 713;iy= 132: 175;bathy{i}(ix,iy)=0; ix= 714: 719;iy= 131: 165;bathy{i}(ix,iy)=0;
ix= 720: 723;iy= 131: 161;bathy{i}(ix,iy)=0; ix= 724: 727;iy= 130: 157;bathy{i}(ix,iy)=0;
ix= 728: 731;iy= 113: 153;bathy{i}(ix,iy)=0; ix= 732: 735;iy= 115: 149;bathy{i}(ix,iy)=0;
ix= 736: 739;iy=  99: 145;bathy{i}(ix,iy)=0; ix= 740: 742;iy=  99: 135;bathy{i}(ix,iy)=0;
ix= 743: 748;iy=  99: 128;bathy{i}(ix,iy)=0; ix= 749: 754;iy=  95: 126;bathy{i}(ix,iy)=0;
ix= 755: 757;iy=  90: 123;bathy{i}(ix,iy)=0; ix= 758: 764;iy=  88: 117;bathy{i}(ix,iy)=0;
ix= 765: 768;iy=  77: 116;bathy{i}(ix,iy)=0; ix= 769: 769;iy=  79:  88;bathy{i}(ix,iy)=0;
ix= 770: 779;iy=  77: 102;bathy{i}(ix,iy)=0; ix= 780: 784;iy=  77: 100;bathy{i}(ix,iy)=0;
ix= 785: 788;iy=  72:  96;bathy{i}(ix,iy)=0; ix= 789: 796;iy=  72:  92;bathy{i}(ix,iy)=0;
ix= 797: 801;iy=  70:  90;bathy{i}(ix,iy)=0; ix= 802: 810;iy=  65:  88;bathy{i}(ix,iy)=0;
ix= 811: 822;iy=  61:  85;bathy{i}(ix,iy)=0; ix= 823: 826;iy=  61:  81;bathy{i}(ix,iy)=0;
ix= 827: 844;iy=  61:  78;bathy{i}(ix,iy)=0; ix= 845: 900;iy=  51:  74;bathy{i}(ix,iy)=0;
ix= 607: 609;iy= 280: 293;bathy{i}(ix,iy)=0; ix= 720: 720;iy= 162: 162;bathy{i}(ix,iy)=0;
ix= 708: 709;iy= 176: 177;bathy{i}(ix,iy)=0; ix= 702: 715;iy= 140: 168;bathy{i}(ix,iy)=0;
ix= 769: 770;iy=  97: 109;bathy{i}(ix,iy)=0; ix= 873: 886;iy=  74:  76;bathy{i}(ix,iy)=0;
ix= 891: 909;iy=  78:  78;bathy{i}(ix,iy)=0; ix= 903: 915;iy=  81:  82;bathy{i}(ix,iy)=0;
ix= 964: 989;iy=  81: 105;bathy{i}(ix,iy)=0; ix= 968: 997;iy= 106: 107;bathy{i}(ix,iy)=0;
ix= 957: 960;iy=  98: 101;bathy{i}(ix,iy)=0; ix= 971:1000;iy= 108: 108;bathy{i}(ix,iy)=0;
ix= 973:1001;iy= 109: 109;bathy{i}(ix,iy)=0; ix= 976:1015;iy= 110: 112;bathy{i}(ix,iy)=0;
ix= 980:1016;iy= 113: 114;bathy{i}(ix,iy)=0; ix= 982: 994;iy= 115: 115;bathy{i}(ix,iy)=0;
ix=1011:1019;iy= 115: 116;bathy{i}(ix,iy)=0; ix=1015:1024;iy= 117: 117;bathy{i}(ix,iy)=0;
ix=1020:1029;iy= 119: 119;bathy{i}(ix,iy)=0; ix=1025:1031;iy= 120: 122;bathy{i}(ix,iy)=0;
ix=1029:1042;iy= 123: 123;bathy{i}(ix,iy)=0; ix=1050:1063;iy= 129: 130;bathy{i}(ix,iy)=0;
ix=1055:1063;iy= 131: 131;bathy{i}(ix,iy)=0; ix=1058:1063;iy= 132: 132;bathy{i}(ix,iy)=0;
ix=1059:1063;iy= 133: 133;bathy{i}(ix,iy)=0; ix=1064:1080;iy=   1: 128;bathy{i}(ix,iy)=0;
ix= 937:1080;iy=   1:  71;bathy{i}(ix,iy)=0; ix= 664: 665;iy= 203: 204;bathy{i}(ix,iy)=0;
ix= 599: 603;iy= 312: 316;bathy{i}(ix,iy)=0; ix= 604: 606;iy= 310: 314;bathy{i}(ix,iy)=0;
ix= 607: 607;iy= 293: 298;bathy{i}(ix,iy)=0; ix= 592: 592;iy= 321: 326;bathy{i}(ix,iy)=0;
ix= 594: 594;iy= 319: 323;bathy{i}(ix,iy)=0; ix= 595: 595;iy= 318: 322;bathy{i}(ix,iy)=0;
ix= 597: 598;iy= 313: 320;bathy{i}(ix,iy)=0; ix= 599: 603;iy= 308: 317;bathy{i}(ix,iy)=0;
ix= 604: 607;iy= 306: 314;bathy{i}(ix,iy)=0; ix= 599: 603;iy= 308: 317;bathy{i}(ix,iy)=0;


figure(1);clf;mypcolor(bathy{i}');caxis([0 1e1]);mythincolorbar;
