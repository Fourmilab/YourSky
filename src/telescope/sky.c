/*

                               Your Sky
                  Virtual Telescope and Horizon View

*/

#include "vplanet.h"
#include "project.h"

#define FONTSCALE   24                  /* Font size to window size default scale factor */

double skyLimitMag = 6.5;               /* Limiting magnitude for stars */
int skyShowName = TRUE;                 /* Show star names in sky map ? */
double skyNameMag = 3.5;                /* Maximum magnitude to show names in sky */
int skyShowBflam = TRUE;                /* Show Bayer/Flamsteed numbers in sky ? */
double skyBflamMag = 4;                 /* Maximum magnitude to show Bayer/Flamsteed in sky */
int skyShowDeep = TRUE;                 /* Show deep sky objects in sky ? */
double skyDeepMag = 6.5;                /* Maximum magnitude to show deep sky objects in sky */
int telSouthUp = FALSE;                 /* South up in telescope ? */
int skyShowConstellations = TRUE;       /* Show constellations */
int skyShowConbounds = TRUE;            /* Show constellation boundaries ? */
int skyShowConnames = TRUE;             /* Show constellation names ? */
int skyAlignConnames = FALSE;           /* Align constellation names ? */
int skyAbbreviateConnames = FALSE;      /* Abbreviate constellation names ? */
int skyShowCoords = TRUE;               /* Show coordinate grid ? */
int skyShowPlanets = TRUE;              /* Show solar system objects ? */
int skyColourScheme = SCS_COLOUR;       /* Colour scheme for sky */
double skyFontScale = 1.0;              /* Text font scale factor */
int telShowMag = FALSE;                 /* Show magnitudes in telescope ? */
double telShowMagMax = -1.5, telShowMagMin = 6; /* Telescope show magnitude limits */
int Flip = 1;                           /* 1 if Northern hemisphere, -1 if Southern */
int flipNorthSouth = FALSE;             /* Flip north and south from default above */
static double skyLham;                  /* Local hour angle offset at last sky map generation */
#define Power1FOV   45.0                /* Field of view for telescope power 1 */
double telra = 0, teldec = 0, telalt = 0, telazi = 0;   /* Telescope aim point */
double telfov = Power1FOV, telpower = 1, telimag = 5.5;  /* Telescope settings */
double horazi = 0;                      /* Horizon azimuth */
int skyMode = paintTEL;                 /* Type of map desired */
int horShowTerrain = TRUE;              /* Show terrain at horizon ? */
int horShowScenery = TRUE;              /* Show scenery at horizon ? */
double horSceneryRoughness = 0.7;       /* Fractal dimension of terrain at horizon */

#define ScaleFont(x) ((int) ((x) * skyFontScale))

static int precessionCalculation = PrecAuto; /* Precession calculation mode */

struct starMapData {
    unsigned short lon;
    short lat;
    unsigned short mag;
    unsigned char spectral;
};


static double faketime;               /* Julian date for which map is bring made */
double lham;                          /* Local hour angle in radians */

#define BrightestMag -1
#define DimmestMag  18
#define BinsPerMag  2

#ifdef ForceTime
#define ssavetime(b, o, g, u, s)      /* Avoid displaying bogus real time */
#else
#define ForceTime()
#endif

/*  OPENRSC  --  Open access to a an embedded CSV resource.  */

static FILE *rscFP = NULL;

static int openRSC(char *rscname)
{
    char fname[132];

    assert(rscFP == NULL);
    strcpy(fname, rscname);
    strcat(fname, ".csv");
    rscFP = fopen(locfile(fname), "r");
    assert(rscFP != NULL);
    return rscFP != NULL;
}

/*  FGETSRSC  --  Get next line from resource file.  */

static char *fgetsRSC(char *cp, int len)
{
    assert(rscFP != NULL);
    return fgets(cp, len, rscFP);
}

/*  CLOSERSC  --  Close resource file.  */

static void closeRSC(void)
{
    assert(rscFP != NULL);
    fclose(rscFP);
    rscFP = NULL;
}

/*  CALCALTAZ  --  Transform from equatorial to alt-azimuth
                   coordinates for horizon view.  */

static void calcAltAz(double dec, double ra, double *alt, double *az)
{
    double lha = lham - dtr(ra);

    *az = fixangle(180 - rtd(atan2(sin(lha), cos(lha) * sin(dtr(siteLat)) -
                            tan(dtr(dec)) * cos(dtr(siteLat)))));
    *alt = rtd(asin(sin(dtr(siteLat)) * sin(dtr(dec)) +
                            cos(dtr(siteLat)) * cos(dtr(dec)) * cos(lha)));
}

/*  ALTAZ  --  Perform alt-azimuth transformation in place when
               horizon view is being generated.  */

static void altaz(double *dec, double *ra)
{
    if (skyMode == paintHOR) {
        double rra, rdec;

        calcAltAz(*dec, *ra, &rdec, &rra);
        *dec = rdec;
        *ra = rra;
    }
}

/*  Note: plotLine may be used only to plot segments of great circles.
    To plot other lines, call clipr_xform() and drawcurveline() yourself
    with FALSE great_circle arguments.  */

static void plotLine(double fdec, double fra, double tdec, double tra)
{
    int vx, vy, vx2, vy2;
    double cdec1, cra1, cdec2, cra2;

/* fprintf(stderr, "plotLine(%.2f, %.2f %.2f, %.2f)\n", fdec, fra, tdec, tra); */
    altaz(&fdec, &fra);
    altaz(&tdec, &tra);
#define GreatCircle TRUE
    if (clipr_xform(fdec, fra, tdec, tra, &vx, &vy, &vx2, &vy2, GreatCircle,
        &cdec1, &cra1, &cdec2, &cra2)) {
        drawcurveline(cdec1, cra1, cdec2, cra2, vx, vy, vx2, vy2, 0, GreatCircle, 0);
    }
}

/* INITIALISESKYTRANSFORM  --  Initialise the sky map transform but do
                               not actually paint the map.  This is used
                               in stateless mode where generation of the
                               image map requires knowledge of the inverse
                               transform even though we have not actually
                               generated the image in that run. */

void initialiseSkyTransform(double julianDate, int imgsize)
{
    int pflip, precess;
    double igmst;

    faketime = julianDate;
    Flip = ((siteLat >= 0) ? 1 : -1) * (flipNorthSouth ? -1 : 1);
    if ((precessionCalculation == PrecAlways) ||
        ((precessionCalculation == PrecAuto) && (abs(PrecEpoch - faketime) >
            (PrecYears * 365.25)))) {
        definePrecession(2000.0 + ((faketime - PrecEpoch) / 365.25));
        precess = TRUE;
    } else {
        precess = FALSE;
    }
#define Prd(x, y) if (precess) { precessObject(x, y, &x, &y); }


    if (skyMode == paintTEL) {
        igmst = gmst(faketime);
        lham = skyLham = dtr((igmst * 15) - siteLon);
        telwin.width = imgsize;
        telwin.height = imgsize;
        telwin.x_offset = 0;
        telwin.y_offset = 0;
        telwin.proj_mode = ((telfov / Power1FOV) < 1.5) ? GNOMONIC : STEREOGR;
        pflip = telwin.invert = !flipNorthSouth;
        telwin.mirror = !telwin.invert;
        telwin.racen = telra;
        telwin.dlcen = teldec;
        telwin.scale = telfov;
        initxform(&telwin);
    } else {
        igmst = gmst(faketime);
        lham = skyLham = dtr((igmst * 15) - siteLon);
        telwin.width = imgsize;
        telwin.height = imgsize;
        telwin.x_offset = 0;
        telwin.y_offset = 0;
        telwin.proj_mode = GNOMONIC;
        pflip = telwin.invert = TRUE;
        telwin.mirror = FALSE;
        telwin.racen = -horazi;
        telwin.dlcen = (telfov / 2 * max(telwin.width, telwin.height)) / telwin.width;
        telwin.scale = telfov;
        initxform(&telwin);
    }
}

/* PAINTSKY  --  Paint the sky map.  */

void paintSky(double julianDate, int imgsize)
{
    int i, vx, vy, vis, vx2, vy2, pflip, precess;
    double igmst, ra, dec, mag;
    unsigned char smex[4];

    faketime = julianDate;
    Flip = ((siteLat >= 0) ? 1 : -1) * (flipNorthSouth ? -1 : 1);
    if ((precessionCalculation == PrecAlways) ||
        ((precessionCalculation == PrecAuto) && (abs(PrecEpoch - faketime) >
            (PrecYears * 365.25)))) {
        definePrecession(2000.0 + ((faketime - PrecEpoch) / 365.25));
        precess = TRUE;
    } else {
        precess = FALSE;
    }
#define Prd(x, y) if (precess) { precessObject(x, y, &x, &y); }

    if (skyMode == paintTEL) {
        igmst = gmst(faketime);
        lham = skyLham = dtr((igmst * 15) - siteLon);
        telwin.width = imgsize;
        telwin.height = imgsize;
        telwin.x_offset = 0;
        telwin.y_offset = 0;
        telwin.proj_mode = ((telfov / Power1FOV) < 1.5) ? GNOMONIC : STEREOGR;
/* fprintf(stderr, "telfov = %.2f  Power1FOV = %.2f, Ratio = %.2f  Mode = %s\n",
    telfov, Power1FOV, (telfov / Power1FOV), telwin.proj_mode == GNOMONIC ? "Gnomonic" : "Stereographic"); */
        pflip = telwin.invert = !flipNorthSouth;
        telwin.mirror = !telwin.invert;
        telwin.racen = telra;
        telwin.dlcen = teldec;
        telwin.scale = telfov;
        initxform(&telwin);
    } else {
        igmst = gmst(faketime);
        lham = skyLham = dtr((igmst * 15) - siteLon);
        telwin.width = imgsize;
        telwin.height = imgsize;
        telwin.x_offset = 0;
        telwin.y_offset = 0;
        telwin.proj_mode = GNOMONIC;
        pflip = telwin.invert = TRUE;
        telwin.mirror = FALSE;
        telwin.racen = -horazi;
        telwin.dlcen = (telfov / 2 * max(telwin.width, telwin.height)) / telwin.width;
        telwin.scale = telfov;
        initxform(&telwin);
    }

    /* Draw coordinate grid markers, if requested */

    if (skyShowCoords) {
        int i;
        double epsilon, esin, ecos, eqra, eqdec, eqlat, pera, pedec;
#define tickWid 0.75

        setFont("roman", ScaleFont(min(15, (imgsize / 2) / FONTSCALE)),
            0, ALIGN_CENTRE);
        setColour(CschemeD(cDkCyan));
        for (i = -1; i <= 1; i += 2) {
            /* Equinoctual colures */
            plotLine(88.0 * i, 0.0, 90.0 * i, 0.0);
            plotLine(88.0 * i, 180.0, 90.0 * i, 180.0);

            /* Solstitial colures */
            plotLine(88.0 * i, 90.0, 90.0 * i, 90.0);
            plotLine(88.0 * i, 270.0, 90.0 * i, 270.0);
        }

#define axform(lat, lon, x, y, in) { double la, lo; la = (lat); lo = (lon); altaz(&la, &lo); \
                                        xform(la, lo, x, y, in); }

        /* Celestial equator */

        for (i = 0; i < 360; i += 15) {     /* Draw the tick marks at hour angle intervals */
            char hlab[4];
            int vx, vy, vis;

            plotLine(0.0, (double) i, 0.0, i + 15.0);
            plotLine(-tickWid, (double) i, tickWid, (double) i);
            sprintf(hlab, "%dh", i / 15);
            axform(tickWid * (pflip ? 1 : -1), (double) i, &vx, &vy, &vis);
            if (vis) {
                drawText(hlab, vx, vy);
            }

        }

        /* Ecliptic */

        epsilon = dtr(obliqeq(faketime));   /* Get current obliquity of ecliptic */
        esin = sin(epsilon);
        ecos = cos(epsilon);

        setColour(CschemeD(cDkRed));
        pera = pedec = 0.0;     /* Dirty trick: ecliptic intersects equator at 0 longitude ! */
        for (i = 1; i <= 32; i++) {         /* Draw the ecliptic itself */
            eqlat = ((PI * 2) / 32.0) * i;
            eqra = fixangle(rtd(atan2(ecos * sin(eqlat), cos(eqlat))));
            eqdec = rtd(asin(esin * sin(eqlat)));
            plotLine(pedec, pera, eqdec, eqra);
            pera = eqra;
            pedec = eqdec;
        }

        for (i = 0; i < 360; i += 15) {     /* Draw the tick marks at 15 degree intervals */
            char hlab[6];
            int vx, vy, vis;

            eqlat = ((PI * 2) / 360.0) * i;
            pera = fixangle(rtd(atan2((ecos * sin(eqlat) -
                            (tan(dtr(-tickWid)) * esin)), cos(eqlat))));
            pedec = rtd(asin((esin * sin(eqlat) * cos(dtr(-tickWid))) +
                        (sin(dtr(-tickWid)) * ecos)));
            eqra = fixangle(rtd(atan2((ecos * sin(eqlat) -
                            (tan(dtr(tickWid)) * esin)), cos(eqlat))));
            eqdec = rtd(asin((esin * sin(eqlat) * cos(dtr(tickWid))) +
                        (sin(dtr(tickWid)) * ecos)));
            plotLine(pedec, pera, eqdec, eqra);
            sprintf(hlab, "%d\177", i);
            axform(!pflip ? pedec : eqdec, !pflip ? pera : eqra, &vx, &vy, &vis);
            if (vis) {
                drawText(hlab, vx, vy);
            }

        }
#undef tickWid
    }

    /* Draw constellation names, if requested */

    if (skyShowConnames) {
        static char *vfmt = "%d,%u,%s";

        if (openRSC("cnames")) {
            char cl[80], cn[80];
            char *cwhich, *ca;
            unsigned int tra;
            int tdec;

            if (!skyAlignConnames) {
                setFont("roman", ScaleFont(min(18, (imgsize / 2) / FONTSCALE)),
                    0, ALIGN_CENTRE | ALIGN_MIDDLE);
            }
            setColour(CschemeD(cYellow));
            while (fgetsRSC(cl, sizeof cl) != NULL) {
                cl[strlen(cl) - 1] = EOS;

                if (strlen(cl) == 0 || isspace(cl[0]) || cl[0] == ';') {
                    continue;
                }
                sscanf(cl, vfmt, &tra, &tdec, cn);
                if (tra == 0 && tdec == 0) {
                    break;
                }
                if (strchr(cn, '_') != NULL) {
                    *strchr(cn, '_') = ' ';
                }
                /* Parse abbreviation and full name */
                cwhich = strchr(cn, ',');
                assert(cwhich != NULL);
                *cwhich = 0;
                ca = cwhich + 1;
                cwhich = skyAbbreviateConnames ? ca : cn;
                ra = tra / (24000.0 / 360.0);
                dec = tdec / (9000.0 / 90.0);
                Prd(ra, dec);
                axform(dec, ra, &vx, &vy, &vis);
                if (vis) {
                    if (skyAlignConnames) {
                        int iangle;
                        double eangle;

                        vx2 = vx - (imgsize / 2);
                        vy2 = vy - (imgsize / 2);
                        if (vx2 == vy2) {
                            eangle = 0;
                        } else {
                            eangle = rtd(atan2((double) vy2, (double) vx2));
                            if (eangle >= 0) {
                                eangle -= 90;
                            } else {
                                eangle += 90;
                            }
                        }
                        eangle = - eangle;
                        iangle = (int) eangle;
                        setFont("roman", ScaleFont(min(18, (imgsize / 2) / FONTSCALE)),
                            iangle, ALIGN_CENTRE);
                    }
                    drawText(cwhich, vx, vy);
                }
            }
            closeRSC();
        }
    }

    /* Draw constellation outlines, if requested */

    if (skyShowConstellations) {
        static char *vfmt = "%3s,%u,%d,%u,%d";

        if (openRSC("conlines")) {
            char cl[80], cn[10];
            unsigned int fra, tra;
            int fdec, tdec;
            double ra2, dec2, cra1, cdec1, cra2, cdec2;

            setColour(CschemeD(cGrey));
            while (fgetsRSC(cl, sizeof cl) != NULL) {
                cl[strlen(cl) - 1] = EOS;

                if (strlen(cl) == 0 || isspace(cl[0]) || cl[0] == ';') {
                    continue;
                }
                sscanf(cl, vfmt, cn, &fra, &fdec, &tra, &tdec);
                if (fra == 0 && tra == 0) {
                    break;
                }
                ra = fra / (24000.0 / 360.0);
                dec = fdec / (9000.0 / 90.0);
                Prd(ra, dec);
                ra2 = tra / (24000.0 / 360.0);
                dec2 = tdec / (9000.0 / 90.0);
                Prd(ra2, dec2);
                altaz(&dec, &ra);
                altaz(&dec2, &ra2);
                if (clipr_xform(dec, ra, dec2, ra2, &vx, &vy, &vx2, &vy2, FALSE,
                    &cdec1, &cra1, &cdec2, &cra2)) {
                    drawVector(vx, vy, vx2, vy2);
                }
            }
            closeRSC();
        }
    }

    /* Draw constellation boundaries, if requested */

    if (skyShowConbounds) {
        static char *vfmt = "%d,%u,%d";

        if (openRSC("cbounds")) {
            char cl[80];
            unsigned int fra = 0, tra;
            int mdraw, fdec = 0, tdec;
            double ra2, dec2, cra1, cdec1, cra2, cdec2;

            setColour(CschemeD(cDkGreen));
            while (fgetsRSC(cl, sizeof cl) != NULL) {
                cl[strlen(cl) - 1] = EOS;

                if (strlen(cl) == 0 || isspace(cl[0]) || cl[0] == ';') {
                    continue;
                }
                sscanf(cl, vfmt, &mdraw, &tra, &tdec);
                if (mdraw == 2) {
                    break;
                } else if (mdraw) {
                    ra = fra / (24000.0 / 360.0);
                    dec = fdec / (9000.0 / 90.0);
                    Prd(ra, dec);

                    ra2 = tra / (24000.0 / 360.0);
                    dec2 = tdec / (9000.0 / 90.0);
                    Prd(ra2, dec2);
                    if (skyMode == paintHOR) {
                        int n;
                        double alt = dec, az = ra, alt2 = dec2, az2 = ra2;

                        altaz(&alt, &az);
                        altaz(&alt2, &az2);
                        if (clipr_xform(alt, az, alt2, az2, &vx, &vy, &vx2, &vy2, FALSE,
                            &cdec1, &cra1, &cdec2, &cra2)) {
                            if (ra == ra2) {
                                /* No need to curve great circles */
                                drawVector(vx, vy, vx2, vy2);
                            } else {
                                /* Make sure we go the short way around the circle */
                                if ((ra - ra2) > 180.0) {
                                    ra = ra - 360.0;
                                } else if ((ra2 - ra) > 180.0) {
                                    ra2 = ra2 - 360.0;
                                }
                                for (n = 0; n < 10; n++) {
                                    alt = dec + n * ((dec2 - dec) / 10);
                                    az = ra + n * ((ra2 - ra) / 10);
                                    alt2 = dec + (n + 1) * ((dec2 - dec) / 10);
                                    az2 = ra + (n + 1) * ((ra2 - ra) / 10);
                                    altaz(&alt, &az);
                                    altaz(&alt2, &az2);
                                    if (clipr_xform(alt, az, alt2, az2, &vx, &vy, &vx2, &vy2, FALSE,
                                        &cdec1, &cra1, &cdec2, &cra2)) {
                                        drawVector(vx, vy, vx2, vy2);
                                    }
                                }
                            }
                        }
                    } else {
                        if (clipr_xform(dec, ra, dec2, ra2, &vx, &vy, &vx2, &vy2, FALSE,
                            &cdec1, &cra1, &cdec2, &cra2)) {
                            drawcurveline(cdec1, cra1, cdec2, cra2, vx, vy, vx2, vy2, 0, FALSE, 0);
                        }
                    }
                }
                fra = tra;
                fdec = tdec;
            }
            closeRSC();
        }
    }

    /* Scan the star map and draw the stars */

    if (TRUE) {
        struct starMapData sd;
        struct starMapData *smp = &sd;
        unsigned char *yp = starcat;
        int imag;
        short ipmra, ipmdec;
        unsigned int ilimag;


        ilimag = (unsigned int) ((skyLimitMag + 1.5) * 100); /* For quick tests against limiting magnitude */
        while (TRUE) {
            char *sname;

            memset(smex, 0, sizeof smex);

            /* Copy the compressed star map data to structure sd in a
               byte-order invariant manner. */

            sd.lon = (yp[1] << 8) | yp[0];
            sd.lat = (yp[3] << 8) | yp[2];
            sd.mag = (yp[5] << 8) | yp[4];
            sd.spectral = yp[6];
            yp += 7;

            sname = (char *) yp;
            if (smp->mag & 0x2000) {
                memcpy(smex, sname, 3);
                sname += 3;
            }
            if (smp->mag & 0x1000) {
                ipmra = (sname[1] << 8) | sname[0];
                ipmdec = (sname[3] << 8) | sname[2];
                sname += 4;
            } else {
                ipmra = ipmdec = 0;
            }
            ra = smp->lon / (65536.0 / 360.0) +
                (((faketime - PrecEpoch) / (JulianCentury / 100)) * (ipmra / (1000.0 * 60 * 60)));
            dec = smp->lat / (32767.0 / 90.0) +
                (((faketime - PrecEpoch) / (JulianCentury / 100)) * (ipmdec / (1000.0 * 60 * 60)));
            Prd(ra, dec);

            axform(dec, ra, &vx, &vy, &vis);
            if (vis) {
                int named = FALSE;

                mag = ((smp->mag & 0xFFF) / 100.0) - 1.5;

                /* If star is bright enough to meet the "show name" criterion
                   and it has a name, display it. */

                setColour(CschemeD(cPink));
                if ((smp->mag & 0x8000) && skyShowName && (mag < skyNameMag)) {
                    setFont("roman", ScaleFont(min(15, (imgsize / 2) / FONTSCALE)), 0, 0);
                    drawText(sname, vx + 8, vy + 2);
                    named = TRUE;
                }

                /* See if the Bayer/Flamsteed code should be drawn */

                if ((smex[0] != 0) && skyShowBflam && (mag < skyBflamMag)) {
                    setFont((smex[0] < 32) ? "greek" : "roman",
                        ScaleFont(min(15, (imgsize / 2) / FONTSCALE)), 0, 0);
                    if (smex[0] < 32) {
                        smex[0] += ('a' - 1);
                    }
                    drawText((char *) smex, vx + (named ? -16 : 8), vy + 2);
                }


                /* See if the magnitude should be plotted beneath the star */

                if (telShowMag && (mag >= min(telShowMagMin, telShowMagMax)) &&
                        (mag <= max(telShowMagMin, telShowMagMax))) {
                    char maggit[20];
                    int im = sgn(mag) * (int) ((abs(mag) + 0.05) * 10);

                    setColour(CschemeD(cLtGrey));
                    setFont("roman", ScaleFont(min(15, (imgsize / 2) / FONTSCALE)),
                        0, ALIGN_CENTRE | ALIGN_TOP);
                    sprintf(maggit, "%d", im);
                    drawText(maggit, vx, vy + 4);
                }

                /* Finally, paint the star according to its magnitude and the quality setting */

#define nStarIcons  9
                imag = ((int) nStarIcons) - ((int) (mag - BrightestMag));
                if (skyLimitMag < (nStarIcons - 2)) {
                    imag -= (nStarIcons - ((int) skyLimitMag)) - 1;
                }
                imag = min(max(imag, 0), nStarIcons - 1);
                drawIcon("stars", StarIcon(imag), vx - 16, vy - 16);
            }

            /* Quit if we hit the end of file or exceed the limiting magnitude.
               Note the assumption that the catalogue is sorted by magnitude! */

            if ((smp->mag & 0x4000) || ((smp->mag & 0xFFF) > ((int) ilimag))) {
                break;
            }
            yp = (((unsigned char *) sname) + ((smp->mag & 0x8000) ? (strlen(sname) + 1) : 0));
        }
    }

    /* Plot deep sky objects, if requested */

    if (skyShowDeep) {
        static char *dcode = "DKDNDSEGGCIGOCPNQSSG", *vfmt = "%lf%c%lf%c%lf%c%s";

        setFont("roman", ScaleFont(min(15, (imgsize / 2) / FONTSCALE)), 0, 0);

        if (openRSC("deepobj")) {
            char cl[132], cn[80], dc;
            double ra, dec, mag;

            while (fgetsRSC(cl, sizeof cl) != NULL) {
                cl[strlen(cl) - 1] = EOS;

                if (strlen(cl) == 0 || isspace(cl[0]) || cl[0] == ';') {
                    continue;
                }
                if (sscanf(cl, vfmt, &ra, &dc, &dec, &dc, &mag, &dc, cn) < 7) {
                    continue;
                }
                if (mag == -99 || mag > skyDeepMag) {
                    break;
                }
                Prd(ra, dec);
                axform(dec, ra, &vx, &vy, &vis);
                if (vis) {
                    char *objclass, *popname;
                    int i, di;

                    popname = strchr(cn, ',');
                    if (popname != NULL) {
                        *popname++ = EOS;
                        if ((objclass = strchr(popname, ',')) != NULL) {
                            *strchr(popname, ',') = EOS;
                        }
                        if (objclass != NULL) {
                            objclass++;
                            if (strchr(objclass, ',') != NULL) {
                                *strchr(objclass, ',') = EOS;
                            }
                            di = 0;
                            for (i = 0; i < strlen(dcode) / 2; i++) {
                                if (strncmp(objclass, dcode + (i * 2), 2) == 0) {
                                    di = i + 1;
                                    break;
                                }
                            }
                            if (di != 0) {
                                char *dname = strlen(popname) > 0 ? popname : cn;

                                drawIcon("deepsky", DeepIcon(di - 1), vx - 16, vy - 16);
                                drawText(dname, vx + 8, vy + 2);
                            }
                        }
                    }

                }
            }
        }
    }

    /*  Now paint the Sun, Moon, planets, and asteroid or comet,
        if one is being tracked, on the map.  If the date has been
        set beyond the validity of VSOP87 (A.D. 8000), the Moon
        and planets are never plotted.  */

    if (skyShowPlanets && (faketime < VSOP87_INVALID)) {
        for (i = 0; i <= (aTracked ? 10 : 9); i++) {
            if (((i < 9) || (planet_info[i].hrv > 0))) {
/*
if (skyMode == paintHOR) {
                xform(planet_info[i].alt, planet_info[i].az, &vx, &vy, &vis);
printf("<! i = %d hrv = %.2f xform %.3f %.3f altaz %d %d, vis=%d -->\n", i, planet_info[i].hrv, planet_info[i].alt, planet_info[i].az, vx, vy, vis);
} else {
*/
                ra = dtr(planet_info[i].ra);
                dec = dtr(planet_info[i].dec);

                /* Note that since planetary positions are computed for the equinox
                   of the date, they are *not* precessed to the current equinox. */

                axform(rtd(dec), rtd(ra), &vx, &vy, &vis);
/*
printf("<! i = %d xform %.3f %.3f altaz %d %d, vis=%d -->\n", i, rtd(dec), rtd(ra), vx, vy, vis);
}
*/
                if (!vis) {
                    continue;
                }

                if (i == 3) {
                    /* Use the moon icon to show current phase */
                    drawIcon("moon", MoonIcon, vx - 16, vy - 13);
                } else {
                    drawIcon("planet",
                        PlanetIcon(i + (((i == 10) && ast_info.cometary) ? 1 : 0)),
                        vx - 16, vy - 16);
                }
            }
        }
    }
#undef Prd
}
