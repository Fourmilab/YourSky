/*

            Your Sky: Telescope/Horizon  --  Definitions

                          by John Walker
                      <http://www.fourmilab.ch/>

*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#include <sys/resource.h>
#include <unistd.h>
#include <limits.h>
#include <stdint.h>
#include <assert.h>

#include "bitmap.h"

#define FALSE   0
#define TRUE    1


#define TERMINC  100                  /* Circle segments for terminator */

#define PI 3.14159265358979323846  /* Assume not near black hole nor in
                                      Tennessee */
#define V       (void)

#define EOS     '\0'

/* Determine number of elements in an array */

#define ELEMENTS(array) (sizeof(array)/sizeof((array)[0]))

/*  Frequently used astronomical constants */

#define J2000               2451545.0       /* Julian day of J2000 epoch */
#define JulianCentury       36525.0         /* Days in Julian century */
#define AstronomicalUnit    149597870.0     /* Astronomical unit in kilometres */
#define SunSMAX  (AstronomicalUnit * 1.000001018) /* Semi-major axis of Earth's orbit */
#define EarthRad            6378.14         /* Earth's equatorial radius, km (IAU 1976) */
#define LunatBase           2423436.0       /* Base date for E. W. Brown's numbered
                                               series of lunations (1923 January 16) */
#define SynMonth            29.53058868     /* Synodic month (mean time from new Moon to new Moon) */
#define ModifiedJulian      2400000.5       /* Offset for modified Julian Day numbers */

/*  Precession calculation modes  */

#define PrecAuto        0                   /* Precess if more then PrecYears from PrecEpoch */
#define PrecAlways      1                   /* Always precess */
#define PrecNever       2                   /* Never correct for precession */
#define PrecEpoch       J2000               /* All databases are epoch J2000.0 */
#define PrecYears       25                  /* Consider databases valid for this time around epoch */

/*  Julian day after which VSOP87 is invalid.  */

#define VSOP87_INVALID  4643000.0     /* 8000 January 1 12:00 UTC */

/*  Handy intrinsic functions  */

#define max(x, y) (((x) > (y)) ? (x) : (y))               /* Maximum */
#define min(x, y) (((x) < (y)) ? (x) : (y))               /* Maximum */
#define sgn(x) (((x) < 0) ? -1 : ((x) > 0 ? 1 : 0))       /* Extract sign */
#define abs(x) ((x) < 0 ? (-(x)) : (x))                   /* Absolute val */
#define fixangle(a) ((a) - 360.0 * (floor((a) / 360.0)))  /* Fix angle */
#define fixangr(a)  ((a) - (PI*2) * (floor((a) / (PI*2))))  /* Fix angle in radians */
#define dtr(x) ((x) * (PI / 180.0))                       /* Degree->Radian */
#define rtd(x) ((x) / (PI / 180.0))                       /* Radian->Degree */

/*  Structure for VSOP87 planetary position results.  */

struct planet {                 /* Planet information entry */
    double hlong;               /* FK5 Heliocentric longitude */
    double hlat;                /* FK5 Heliocentric latitude */
    double hrv;                 /* Heliocentric radius vector */
    double dhlong;              /* Dynamical Heliocentric longitude */
    double dhlat;               /* Dynamical Heliocentric latitude */
    double ra;                  /* Apparent right ascension */
    double dec;                 /* Apparent declination */
    double dist;                /* True distance from the Earth */
    double mag;                 /* Approximate magnitude */
    double lha;                 /* Local hour angle */
    double alt;                 /* Altitude above (-below) horizon */
    double az;                  /* Azimuth from South: West positive, East negative */
};
extern struct planet planet_info[11]; /* Calculated planetary information */

extern int aTracked;            /* Tracking an asteroid ? */

struct asteroid_info {          /* Asteroid information */
    int cometary;               /* Nonzero if cometary element format */
    char Name[82];              /* Name and number */
    double MagH;                /* IAU Commission 20 magnitude argument H */
    double MagG;                /* IAU Commission 20 magnitude argument G */
    double SemiMajorAU;         /* Semimajor axis in AU */
    double Eccentricity;        /* Eccentricity of orbit */
    double Inclination;         /* Inclination of orbit to ecliptic */
    double ArgP;                /* Argument of perihelion */
    double LANode;              /* Longitude of the ascending node */
    double mAnomaly;            /* Mean anomaly at the epoch */
    double Epoch;               /* Epoch of elements */
    double PeriDate;            /* Time of perihelion passage */
    double PeriAU;              /* Perihelion distance, AU */
};

extern struct asteroid_info ast_info;   /* Information about currently-tracked asteroid */

/*  Icon number space definitions.  */

#define PlanetIcon(x)   (x)
#define MoonIcon        13
#define DeepIcon(x)     ((x) + 14)
#define StarIcon(x)     ((x) + 24)
#define TerrainIcon(x)  ((x) + 33)

/*  RGB components packed into a long.  */

#define RGB(r, g, b)    (((((r) << 8) | (g)) << 8) | (b))

/*  Select colour based on scheme:

        Cscheme(full_colour, black_on_white, white_on_black, night_vision)
*/

#define Cscheme(a, b, c, d) (skyColourScheme == SCS_COLOUR ? a : \
                              (skyColourScheme == SCS_BLACK_ON_WHITE ? b : \
                                (skyColourScheme == SCS_WHITE_ON_BLACK ? c : d \
                                ) \
                              )   \
                            )

/*  Default colour scheme for material in map.  */

#define CschemeD(a) Cscheme(a, cBlack, cWhite, cRed)

/*  Test mode file extension.  */

#ifdef TESTMODE
#define TESTFLAG ".t"
#else
#define TESTFLAG ""
#endif

/*  Embedded resource databases.  */

extern unsigned char starcat[];       /* Star catalogue */

/* From vplanet.c */

extern char *progpath;          /* Program invocation path */
extern double siteLat, siteLon; /* Observing site latitude and longitude */
extern unsigned char *tpixel;   /* Icon database */
extern int tmrowlen, tmhm1;     /* Icon database shape parameters */
#define PixA(x, y) ((tpixel + ((x) + (((DWORD) (tmhm1 - (y))) * (tmrowlen)))))
#define maxOrbitalElements 10
extern char orbitalElements[maxOrbitalElements][132]; /* Elements of body being tracked */
extern char savedOrbitalElements[maxOrbitalElements][132]; /* Elements submitted with stateless request */
extern char *elementSpecification;  /* Orbital elements supplied by the user from WWW_elements */
extern int transparent, cBlack, cBlue, cGreen, cRed, cBorder, cWhite,  /* Bitmap colour indices */
                        cDkCyan, cDkRed, cYellow, cGrey, cDkGreen,
                        cPink, cDkBlue, cLtGrey;
extern char *locfile(const char *basename);
extern void psrvector(int fx, int fy, int tx, int ty, int colour);
extern void psricon(int iconNo, int ix, int iy);

/* From astrelem.c */

extern int element_rs(char *s);
extern void astrelem(const int preloaded);
extern void print_elements(FILE *fp);

/* From astro.c */

extern double ucttoj(long year, int mon, int mday,
                     int hour, int min, int sec);
extern double jtime(struct tm *t);
extern void jyear(double td, long *yy, int *mm, int *dd);
extern void jhms(double j, int *h, int *m, int *s);
extern double gmst(double jd);
extern void sunpos(double jd, int apparent,
                   double *ra, double *dec, double *rv, double *slong);
extern double phase(double pdate, double *pphase, double *mage, double *dist,
                    double *angdia, double *sudist, double *suangdia);
extern void highmoon(double jd, double *l, double *b, double *r);
extern void nutation(double jd, double *deltaPsi, double *deltaEpsilon);
extern void ecliptoeq(double jd, double Lambda, double Beta, double *Ra, double *Dec);
extern double obliqeq(double jd);
extern void definePrecession(double targetEpoch);
extern void precessObject(double ira, double idec, double *ora, double *odec);

/* From cometel.c */

extern void cometel(char *firstline);

/* From csvelements.c */

extern int csvElements(char *s);

/* From gifout.c */

extern int gifout(int cols, int rows, int rowlen, RGBQUAD *colmap, int colors,
                  int transparent, unsigned char *bitmap, FILE *outfile);

/* From graphics.c */

#define ALIGN_LEFT      0             /* Left justify */
#define ALIGN_CENTRE    1             /* Centre horizontally */
#define ALIGN_RIGHT     2             /* Right justify */

#define ALIGN_BASELINE  0             /* Start at baseline */
#define ALIGN_TOP       4             /* Align with top of extents box */
#define ALIGN_BOTTOM    8             /* Align with bottom of extents box */
#define ALIGN_MIDDLE   16             /* Centre vertically */

extern void setWindowOffset(int x, int y);
extern int setColour(int newColour);
extern void drawVector(int x1, int y1, int x2, int y2);
extern void setFont(char *fontname, int size, int angle, int align);
extern void drawText(char *text, int x, int y);
extern void drawIcon(char *iconFile, int iconNo, int x, int y);

/* From htmlutil.c */

extern void write_html_content(FILE *fp, const char *s);
extern char *escape_html_content(char *s);

/* From jplelements.c */

extern int jplElements(const char *s);

/* From packargs.c */

extern char *packargs(int argc, char *argv[]);
extern char *unpackargs(const char *oblock);

/* From sky.c */

#define paintTEL    0                 /* Telescope */
#define paintSKY    1                 /* Sky */
#define paintHOR    2                 /* Horizon */

extern int skyMode;                   /* Current window being plotted */
extern double skyLimitMag;            /* Limiting magnitude for stars */
extern int skyShowName;               /* Show star names in sky map ? */
extern double skyNameMag;             /* Maximum magnitude to show names in sky */
extern int skyShowBflam;              /* Show Bayer/Flamsteed numbers in sky ? */
extern double skyBflamMag;            /* Maximum magnitude to show Bayer/Flamsteed in sky */
extern int skyShowDeep;               /* Show deep sky objects in sky ? */
extern double skyDeepMag;             /* Maximum magnitude to show deep sky objects in sky */
extern int skyShowConstellations;     /* Show constellations */
extern int skyShowConbounds;          /* Show constellation boundaries ? */
extern int skyShowConnames;           /* Show constellation names ? */
extern int skyAbbreviateConnames;     /* Abbreviate constellation names ? */
extern int skyAlignConnames;          /* Align constellation names ? */
extern int skyShowCoords;             /* Show coordinate grid ? */
extern int skyShowPlanets;            /* Show solar system objects ? */
extern int telShowMag;                /* Show magnitudes in telescope ? */
extern double telShowMagMax, telShowMagMin; /* Telescope show magnitude limits */
extern int skyColourScheme;           /* Colour scheme for sky */
#define SCS_COLOUR          0             /* Normal colour display */
#define SCS_BLACK_ON_WHITE  1             /* Black print on white background */
#define SCS_WHITE_ON_BLACK  2             /* White print on black background */
#define SCS_NIGHT_VISION    3             /* Red to preserve night vision */
extern double skyFontScale;           /* Text font scale factor */
extern int Flip;                      /* 1 if Northern hemisphere, -1 if Southern */
extern int flipNorthSouth;            /* Flip north and south from default above */
extern double telShowMagMax, telShowMagMin;    /* Telescope show magnitude limits */
extern double telra, teldec, telalt, telazi;   /* Telescope aim point */
extern double telfov, telpower, telimag;       /* Telescope settings */
extern double horazi;                 /* Horizon azimuth */
extern int horShowTerrain;            /* Show terrain at horizon ? */
extern int horShowScenery;            /* Show scenery at horizon ? */
extern double horSceneryRoughness;    /* Fractal dimension of terrain at horizon */
extern double lham;                   /* Local hour angle in radians */
extern void initialiseSkyTransform(double julianDate, int imgsize);
extern void paintSky(double julianDate, int imgsize); /* Paint star map */

/* From vsop87.c */

#define FULL_PLANET_INFO    0x8000          /* Calculate complete high-precision planet info */
extern void planets(double jd, int which);  /* Update planetary positions */
extern void nutation(double jd, double *deltaPsi, double *deltaEpsilon);

/* From planetp.c */

extern void updatePlanet(double jd, FILE *ofile, char *obSite, char *qelt);
extern void updatePlanetComplete(double jd, FILE *ofile, char *obSite, char *qelt);
extern void calcPlanet(double jd);

/* From pluto.c */

extern int pluto(double jd, double *l, double *b, double *r); /* Special calculation for Pluto */

/* From psrtext.c */

extern void psr_text(char *s, int x, int y, int height, int angle);
extern void psr_text_box(char *s, int height, int angle,
                         int *left, int *top, int *right, int *bottom); /* Compute text bounding box */

/* From asteroid.c */

extern void trackAsteroid(double jd, double *ra, double *dec, double *dist,
                          double *hlong, double *hlat, double *hrv, int quick);

/* From sunmoon.c */

extern void initmoon(int iconNo);
extern void updMoonIcon(double jd);

/* From terrain.c */

extern void drawTerrain(int tmsize, double jd, double azimuth,
                        double roughness, int scenery);

/* From uncgi.c */

extern void uncgi(void);

/* From strlcpy.c */

size_t strlcpy(char *dst, const char *src, size_t siz);
