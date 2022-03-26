/*

            Your Sky -- Telescope/Horizon View Server Program

                            by John Walker
                       http://www.fourmilab.ch/

*/

#include "vplanet.h"
#include "project.h"

/* #define DBLOG  */
/* #define ShowCPUtime  */

/*  If CACHE_WARNING is defined, generated HTML will contain a warning
    to users not to link to the ephemeral cache files.  */
/* #define CACHE_WARNING */ /**/

#define TimeLimit   25                /* CPU time limit in seconds */

#define MAX_IMAGE_SIZE 4096           /* Maximum image size */

#ifdef TimeLimit
#define __USE_XOPEN_EXTENDED
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>
#endif

double siteLat = 47.0, siteLon = -7.0;

char orbitalElements[maxOrbitalElements][132];        /* Elements of body being tracked */
char savedOrbitalElements[maxOrbitalElements][132];   /* Elements submitted with dynamic request */

                                      /* Colour indices:  These are found in the icon
                                         bitmap (in which all must be used) and then
                                         subsequently used for material we draw. */
int transparent = -1, cBlack, cBlue, cGreen, cRed, cBorder = -1, cWhite,
                      cDkCyan, cDkRed, cYellow, cGrey, cDkGreen,
                      cPink, cDkBlue, cLtGrey;

static int dto = 0;                   /* Date/time option */

static BITMAPFILEHEADER bfh;
static struct bmih {
    BITMAPINFOHEADER bmiHeader;
    RGBQUAD bmiColors[256];
} bmi, *tmbmap;
static int bmRowLen;
static unsigned char *bmBits = NULL, *tmbits;
static unsigned char ct[4];

static double jt, sunra, sundec, sunlong;
static struct tm ctim;
char *progpath;

static int directCall = FALSE;        /* Were we invoked directly, without uncgi first ? */
static int embeddedImage = FALSE;     /* Is this an embedded image specified with a "?di=" block ? */
char *elementSpecification = NULL;    /* Contents of orbit elements textbox in the request */

static char viewdesc[132], viewdesch[140], dtutc[80];
static int dumpelem = FALSE;          /* Dump orbital elements ? */
#ifdef SAVESET
static int bookmark = FALSE;          /* Save parameters for bookmark ? */
static int gotten = FALSE;            /* Was "get" method used ? */
#endif

/*  Cardinal point on the compass  */

static char *cardinals[] = { "N", "NNE", "NE", "ENE", "E",
                             "ESE", "SE", "SSE", "S",
                             "SSW", "SW", "WSW", "W",
                             "WNW", "NW", "NNW" };


extern char **environ;                /* Operating system environment strings */

#ifdef TimeLimit

/*  DINGALING  --  Handle SIGALRM when time runs out.  */

static void dingaling(int sig)
{
    struct rusage u;

    if (sig == SIGALRM) {
        getrusage(RUSAGE_SELF, &u);
/* printf("Dingaling: utime = %ld  stime = %ld  ttime = %ld\n", u.ru_utime.tv_sec, u.ru_stime.tv_sec, u.ru_utime.tv_sec + u.ru_stime.tv_sec); */
        if ((u.ru_utime.tv_sec + u.ru_stime.tv_sec) > TimeLimit) {
            fprintf(stderr, "Yourtel/Yourizon: time limit (%d seconds) exceeded.\n", TimeLimit);
            printf("<h1><font color=\"#FF0000\">Time limit (%d seconds) exceeded.</font></h1>\n", TimeLimit);
            exit(0);
        }
        alarm(TimeLimit);             /* Wind the cat */
    }
}
#endif

/*  LOAD_BITMAP  --  Load a Microsoft Windows .BMP file into memory.  */

#define rB(x)   (x) = getc(fp)
#define rS(x)   fread(ct, 2, 1, fp); (x) = ((ct[1] << 8) | ct[0])
#define rL(x)   fread(ct, 4, 1, fp); (x) = (((long) ct[3] << 24) | ((long) ct[2] << 16) | (ct[1] << 8) | ct[0])

static void load_bitmap(char *bmpfile)
{
    FILE *fp = fopen(bmpfile, "r");

    if (bmBits != NULL) {
        free(bmBits);
        bmBits = NULL;
    }
    if (fp != NULL) {
        rS(bfh.bfType);
        rL(bfh.bfSize);
        rS(bfh.bfReserved1);
        rS(bfh.bfReserved2);
        rL(bfh.bfOffBits);

        rL(bmi.bmiHeader.biSize);
        rL(bmi.bmiHeader.biWidth);
        rL(bmi.bmiHeader.biHeight);
        rS(bmi.bmiHeader.biPlanes);
        rS(bmi.bmiHeader.biBitCount);
        rL(bmi.bmiHeader.biCompression);
        rL(bmi.bmiHeader.biSizeImage);
        rL(bmi.bmiHeader.biXPelsPerMeter);
        rL(bmi.bmiHeader.biYPelsPerMeter);
        rL(bmi.bmiHeader.biClrUsed);
        rL(bmi.bmiHeader.biClrImportant);

        assert(bmi.bmiHeader.biBitCount == 8);
        assert(bmi.bmiHeader.biCompression == BI_RGB);

        if (bmi.bmiHeader.biClrUsed == 0) {
            bmi.bmiHeader.biClrUsed = 1 << bmi.bmiHeader.biBitCount;
        }

        /* Read the colour palette table. */

        fread(&bmi.bmiColors[0], sizeof(RGBQUAD), bmi.bmiHeader.biClrUsed, fp);

        /* Compute the row length and read the bitmap. */

        bmRowLen = (bmi.bmiHeader.biWidth + 3) & ~3;

        fseek(fp, bfh.bfOffBits, SEEK_SET);
        bmBits = (unsigned char *) malloc(bmRowLen * bmi.bmiHeader.biHeight);
        fread(bmBits, bmRowLen * bmi.bmiHeader.biHeight, 1, fp);

        fclose(fp);
    }
else { fprintf(stderr, "Cannot open bitmap %s\n", bmpfile); }
}

#ifdef PPM_SUPPORT

/*  DUMP_PPM  --  Dump image in buffer in PPM format.  */

static void dump_ppm(char *fname, struct bmih *bi, unsigned char *bits)
{
    FILE *fp;
    int i, j;

    if (fname[0] == '-') {
        fp = stdout;
    } else {
        fp = fopen(fname, "w");
    }
    assert(fp != NULL);
    fprintf(fp, "P6 %ld %ld %d\n", bi->bmiHeader.biWidth, bi->bmiHeader.biHeight, 255);
    for (i = 0; i < bi->bmiHeader.biHeight; i++) {
        unsigned char *px = bits +
            (((bi->bmiHeader.biHeight - 1) - i) *
            ((bi->bmiHeader.biWidth + 3) & ~3));

        for (j = 0; j < bi->bmiHeader.biWidth; j++) {
            int p = *px++;

            putc(bi->bmiColors[p].rgbRed, fp);
            putc(bi->bmiColors[p].rgbGreen, fp);
            putc(bi->bmiColors[p].rgbBlue, fp);
        }
    }
    if (fp != stdout) {
        fclose(fp);
    }
}
#endif

/*  ESCAPE_URL  --  Perform escaping of forms data for URL.  */

static char *escape_url(char *s)
{
    char *os = (char *) malloc((3 * strlen(s)) + 1), *o;
    unsigned char c;

    if (os == NULL) {
        return NULL;
    }

    /*  Escape any non-alphanumeric characters.  */

    o = os;
    while ((c = (unsigned char) (*s++)) != 0) {
        if (!isalnum(c)) {
            if (c == ' ') {
                *o++ = '+';
            } else {
                sprintf(o, "%%%02X", c);
                o += strlen(o);
            }
        } else {
            *o++ = c;
        }
    }
    *o++ = 0;
    return os;
}

/*  PARSEDEG  --  Parse latitude and longitude degree specifications.  */

static double parseDeg(char *dec)
{
    if ((strchr(dec, 'd') != NULL) ||
        (strchr(dec, 'D') != NULL) ||
        (strchr(dec, ' ') != NULL) ||
        (strchr(dec, '°') != NULL)) {
        double dd = 0, mm = 0, ss = 0;
        char c1, c2;

        sscanf(dec, "%lf%c%lf%c%lf", &dd, &c1, &mm, &c2, &ss);
        return sgn(dd) * (abs(dd) + (mm / 60) + (ss / 3600));
    } else {
        return atof(dec);
    }
}

/*  PARSERA  --  Parse right ascension specification.  */

static double parseRA(char *ra)
{
    double fra;

    if ((strchr(ra, 'h') != NULL) ||
        (strchr(ra, 'H') != NULL) ||
        (strchr(ra, ' ') != NULL)) {
        double hh = 0, mm = 0, ss = 0;
        char c1, c2;

        sscanf(ra, "%lf%c%lf%c%lf", &hh, &c1, &mm, &c2, &ss);
        fra = (hh + (mm / 60) + (ss / 3600)) * 15;
    } else {
        fra = atof(ra);
    }
    return fra;
}

/*  EDITRA  --  Edit right ascension.  */

static char *editRA(double ra)
{
    double m = fixangle(ra) / 15;
    static char s[40];
    int mm, sec;

    mm = ((int) (m * 60)) % 60;
    sec = (int) fmod(m * 3600, 60.0);
    sprintf(s, "%dh", (int) m);
    if (mm != 0 || sec != 0) {
        sprintf(s + strlen(s), " %dm", mm);
        if (sec != 0) {
            sprintf(s + strlen(s), " %ds", sec);
        }
    }
    return s;
}

/*  LOCFILE  --  Expand local file name to full directory name from
                 which we're executing this program.  */

char *locfile(const char *basename)
{
    static char pn[PATH_MAX];

    strcpy(pn, progpath);
    if (strrchr(pn, '/') != NULL) {
        *(strrchr(pn, '/') + 1) = EOS;
    } else {
        strcpy(pn, "./");
    }
    if (directCall || embeddedImage) {
        strcat(pn, "../cgi-executables/");
    }
    strcat(pn, basename);
    return pn;
}

/*  EDDEG  --  Edit degrees and minutes.  */

static char *eddeg(double ds)
{
    static char buf[4][20];
    static int n = 0;
    long a = (long) ((abs(ds) + 0.00001) * 3600);
    int d, m, s;

    n = (n + 1) % 4;
    d = a / 3600;
    m = (a / 60) % 60;
    s = (a % 60);
    sprintf(buf[n], "%s%d\260", ds < 0 ? "-" : "", d);
    if (m > 0 || s > 0) {
        sprintf(buf[n] + strlen(buf[n]), "%d'", m);
        if (s > 0) {
            sprintf(buf[n] + strlen(buf[n]), "%d&quot;", s);
        }
    }
    return buf[n];
}

/*  EDLAT  --  Edit latitude.  */

static char *edlat(double lat)
{
    static char slat[30];
    double ulat = fixangle(rtd(lat));

    if (ulat >= 180) {
        ulat = -(360 - ulat);
    }
    sprintf(slat, "%s%s", eddeg(abs(ulat)), ulat < 0 ? "S" : "N");
    return slat;
}

/*  EDLON  --  Edit longitude.  */

static char *edlon(double lon)
{
    static char slon[30];
    double ulon = fixangle(rtd(lon));

    if (ulon >= 180) {
        ulon = -(360 - ulon);
    }
    sprintf(slon, "%s%s", eddeg(abs(ulon)), ulon < 0 ? "E" : "W");
    return slon;
}

/*  EDDEC  --  Edit declination.  */

static char *eddec(double lat)
{
    static char slat[30];
    double ulat = lat;

    if (ulat >= 90) {
        ulat = 90 - ulat;
    }
    sprintf(slat, "%s%s", ulat < 0 ? "-" : "+", eddeg(abs(ulat)));
    return slat;
}

/*  EDDECH  --  Edit declination for use in HTML.
                This differs from eddec() above in that
                it uses the HTML "&minus;" entity for
                negative numbers.  */

static char *eddech(double lat)
{
    static char slat[30];
    double ulat = lat;

    if (ulat >= 90) {
        ulat = 90 - ulat;
    }
    sprintf(slat, "%s%s", ulat < 0 ? "&minus;" : "+", eddeg(abs(ulat)));
    return slat;
}

/*  EDVPOS  --  Edit viewing position and return string.  */

static char *edvpos(double tlat, double tlon)
{
    static char vpos[80];

    sprintf(vpos, "%s %s", edlat(tlat), edlon(tlon));

    return vpos;
}

/*  EDITAZI  --  Edit azimuth, including compass cardinal point if
                 equivalent.  */

static char *editazi(double az)
{
    static char tbuf[80];
    double dtol;

#define AZICARDTOL 0.5
    az = fixangle(az);
    if (az > (360 - AZICARDTOL)) {
        az = 360 - az;
    }
    dtol = abs(az - (int) (((int) (az / 22.5) * 22.5)));
    sprintf(tbuf, "%s", eddeg(az));
    if (dtol < 1) {
        sprintf(tbuf + strlen(tbuf), " (%s)", cardinals[(int) (az / 22.5)]);
    }
#undef AZICARDTOL
    return tbuf;
}

/*  PSRVECTOR  --  Draw a vector into the current image array.  */


#define Opix(x, y) ((opixel + ((x) + (((adiheight - 1) - (y)) * (rowlen)))))

static int rowlen, adiwidth, adiheight;
static unsigned char *opixel;
int tmrowlen, tmhm1;
unsigned char *tpixel;

void psrvector(int fx, int fy,
               int tx, int ty, int colour)
{
    int cfx, cfy, ctx, cty;
    int x, y, m, f, xinc, loopcnt, dx, dy;

#ifdef FlipY
    fy = (iarray[1] - 1) - fy;
    ty = (iarray[1] - 1) - ty;
#endif

    if (fy < ty) {
        cfx = fx;
        cfy = fy;
        ctx = tx;
        cty = ty;
    } else {
        cfx = tx;
        cfy = ty;
        ctx = fx;
        cty = fy;
    }
    dy = cty - cfy;
    dx = ctx - cfx;

    if (dx < 0) {
        xinc = -1;
        dx = -dx;
    } else  {
        xinc = 1;
    }

    x = cfx;
    y = cfy;

    if (dx > dy) {
        f = dy - dx / 2;
        m = dx - dy;
        loopcnt = dx + 1;

        while (loopcnt--) {
            if (x >= 0 && x < adiwidth && y >= 0 && y < adiheight) {
                *Opix(x, y) = (unsigned char) colour;
            }
            x += xinc;
            if (f > 0) {
                f -= m;
                y++;
            } else {
                f += dy;
            }
        }
    } else {
        f = -(dy / 2 - dx);
        m = dy - dx;
        loopcnt = dy + 1;

        while (loopcnt--) {
            if (x >= 0 && x < adiwidth && y >= 0 && y < adiheight) {
                *Opix(x, y) = (unsigned char) colour;
            }
            y++;
            if (f > 0) {
                x += xinc;
                f -= m;
            } else {
                f += dx;
            }
        }
    }
}

/*  PSRICON  --  Plot an icon at a specified position in the bitmap.  */

void psricon(int iconNo, int ix, int iy)
{
    int t = iconNo * 32, x, y;

/*
fprintf(stderr, "psricon(%d, %d, %d) \n", iconNo, ix, iy);
fprintf(stderr, "  tmhm1 = %d, tmrowlen = %d\n", tmhm1, tmrowlen);
*/
    for (x = 0; x < 32; x++) {
        int ax = ix + x;

        if (ax >= 0 && ax < adiwidth) {
            for (y = 0; y < 32; y++) {
                int ay = iy + y;

                if (ay >= 0 && ay < adiheight) {
                    unsigned char p = *PixA(x + t, y);

                    if (p != transparent) {
                        *Opix(ax, ay) = p;
                    }
                }
            }
        }
    }
}

static double earthrv;                /* Heliocentric coordinates of the Earth */

/*  INITBMP  --  Allocate and initialise the output bitmap.  */

static void initbmp(int tmwidth, int tmheight,
                   int diwidth, int diheight, int skycolour)
{
    int siwidth = diwidth;
    BITMAPINFOHEADER *bh, *obmap;
    WORD ncol, offbits;

    adiwidth = diwidth;
    adiheight = diheight;
    bh = &bmi.bmiHeader;
    ncol = bh->biClrUsed == 0 ? (1L << bh->biBitCount) : bh->biClrUsed;
    offbits = bh->biSize + ncol * sizeof(RGBQUAD);
    tpixel = bmBits;
    tmrowlen = ((tmwidth + (sizeof(LONG) - 1)) / sizeof(LONG)) * sizeof(LONG);
    tmhm1 = tmheight - 1;

    if (tmbmap != NULL) {
        free(tmbmap);
        tmbmap = NULL;
    }
    rowlen = ((siwidth + (sizeof(LONG) - 1)) / sizeof(LONG)) * sizeof(LONG);
    obmap = (BITMAPINFOHEADER *) malloc(offbits + diheight * rowlen);
    memset((char *) obmap, 0, offbits);            /* Zero bitmap header */
    memcpy((char *) obmap, (char *) bh, offbits);
    obmap->biWidth = siwidth;
    obmap->biHeight = diheight;
    obmap->biSizeImage = diheight * rowlen;
    opixel = ((unsigned char *) (obmap)) + offbits;
    tmbmap = (struct bmih *) obmap;
    tmbits = (unsigned char *) opixel;
    memset((char *) tmbits, skycolour, diheight * rowlen);
}

/*  TH  --  If a horizon view is being generated, patch all
            references to the telescope controls document to
            point at their counterparts for the horizon.  */

static char *TH(char *s)
{
    /* Watch out!  hpat and hpatch must be the same length. */
    static const char hpat[] =   "/telcontrols.",
                      hpatch[] = "/horcontrols.";

    if (skyMode == paintHOR) {
        int hlong = strlen(hpatch);
        int slong = strlen(s) + 1;
        static int sbufl = 0;
        static char *sbuf = NULL;
        char *cp;

        if (slong > sbufl) {
            if (sbuf != NULL) {
                free(sbuf);
            }
            sbufl = slong;
            sbuf = malloc(sbufl);
            if (sbuf == NULL) {
                fprintf(stderr, "Yoursky/Yourizon: Unable to allocate %d byte string replacement buffer.",
                    sbufl);
                abort();
            }
        }
        strcpy(sbuf, s);
        cp = s = sbuf;

        while ((cp = strstr(cp, hpat)) != NULL) {
            memcpy(cp, hpatch, hlong);
            cp += hlong;
        }
    }
    return s;
}

/*  WRITE_CACHE_IMAGE_WARNING  --  Include warning in HTML source to deter
                                   users from linking to images in the cache.  */

#ifdef CACHE_WARNING
static void write_cache_image_warning(FILE *fp)
{
#define W(x)    fprintf(fp, "%s\n", x)
    W("<!--");
    W("                      WARNING!!!");
    W("");
    W("You may be looking at the source for this Your Sky result");
    W("document in order to figure out where the image file comes from");
    W("so you can link to it from your site.  DON'T DO THAT!");
    W("");
    W("The image files generated by Your Sky are stored in");
    W("a temporary \"cache\" directory, and are kept only long enough");
    W("to guarantee that users will be able to download them to their");
    W("browsers.  After 10 minutes or so, images are automatically deleted.");
    W("So, if you link to one of these images, it may work if you try it");
    W("right away, but the link is sure to break before long, leaving an");
    W("ugly broken image on your page.");
    W("");
    W("Rather than linking to the image, load it in your browser and then");
    W("save a local copy on your site and reference it from there.");
    W("If you want to link to an image which will be updated every time");
    W("users request it, create a custom request with:");
    fprintf(fp, "  http://www.fourmilab.ch/yoursky/%scustom.html\n", (skyMode == paintHOR) ? "hor" : "tel");
    W("then copy the URL it generates into a link on your page.");
    W("");
    W("Thank you for your understanding and cooperation.");
    W("-->");
#undef W
}
#endif

/*  WRITEPOST  --  Write balance of HTML file, including controls
                   which permit regenerating the view.  */

static void writepost(double vlat, double vlon, int tmsize, double jt, char *qelt)
{
    int i, j;
    double lat, lon;

    lon = fixangle(rtd(vlon));
    if (lon >= 180) {
        lon = -(360 - lon);
    }
    lat = fixangle(rtd(vlat));
    if (lat >= 180) {
        lat = -(360 - lat);
    }

#define W(x)    printf("%s\n", x)
#define L(x, y)    printf("<a href=\"/yoursky/help/%s.html\"><b>%s:</b></a>", y, x);
#define Chk(x)  ((x) ? "checked=\"checked\" " : "")
#define Sel(x)  ((x) ? " selected=\"selected\"" : "")

        printf(TH("\
<form method=\"post\" name=\"request\" action=\"/cgi-bin/Your%s" TESTFLAG "\">\n\
<center>\n\
\n\
<input type=\"submit\" value=\"Update\" />\n\
\n\
<p />\n\
\n\
<a href=\"/yoursky/help/telcontrols.html\"><em>Explain controls in the following panel.</em></a>\n\
<table border=\"border\" cellpadding=\"5\">\n\
\n\
<tr>\n\
<th>\n\
     <a href=\"/yoursky/help/telcontrols.html#DateTime\"><b>Date and Time</b></a>\n\
</th>\n\
<td>\n\
<table>\n\
<tr>\n\
<td>\n\
<input type=\"radio\" name=\"date\" onclick=\"0\" %svalue=\"0\" /> <a href=\"/yoursky/help/telcontrols.html#Now\">Now</a>\n\
</td>\n\
</tr>\n\
<tr>\n\
<td>\n\
<input type=\"radio\" name=\"date\" onclick=\"0\" %svalue=\"1\" /> <a href=\"/yoursky/help/telcontrols.html#UTC\">Universal time:</a>\n\
</td>\n\
<td>\n\
<input type=\"text\" name=\"utc\" value=\"%s\" size=\"20\" onchange=\"document.request.date[1].checked=true;\" />\n\
</td>\n\
</tr>\n\
<tr>\n\
<td>\n\
<input type=\"radio\" name=\"date\" onclick=\"0\" %svalue=\"2\" /> <a href=\"/yoursky/help/telcontrols.html#Julian\">Julian day:</a>\n\
</td>\n\
<td>\n\
<input type=\"text\" name=\"jd\" value=\"%.5f\" size=\"20\" onchange=\"document.request.date[2].checked=true;\" />\n\
</td>\n\
</tr>\n\
</table>\n\
"),     skyMode == paintHOR ? "horizon" : "tel",
        Chk(dto == 0), Chk(dto == 1), dtutc, Chk(dto == 2), jt);

        if (skyMode == paintHOR) {
            printf("\
\n\
</td>\n\
</tr>\n\
<tr>\n\
<th>\n\
          <a href=\"/yoursky/help/horcontrols.html#ViewPoint\">Viewpoint</a>\n\
</th>\n\
<td>\n\
<table cellpadding=\"3\">\n\
<tr>\n\
<td>\n\
<a href=\"/yoursky/help/horcontrols.html#Azimuth\">Azimuth</a>:\n\
</td>\n\
<td>\n\
<select name=\"azimuth\" size=\"1\" onchange=\"if (form.azimuth.selectedIndex != 0) { form.azideg.value = (45 * (form.azimuth.selectedIndex - 1)); }\">\n\
<option value=\"V\" selected=\"selected\">Degrees:</option>\n\
<option value=\"0\">North</option>\n\
<option value=\"45\">Northeast</option>\n\
<option value=\"90\">East</option>\n\
<option value=\"135\">Southeast</option>\n\
<option value=\"180\">South</option>\n\
<option value=\"225\">Southwest</option>\n\
<option value=\"270\">West</option>\n\
<option value=\"315\">Northwest</option>\n\
</select>\n\
<input type=\"text\" name=\"azideg\" value=\"%s\" size=\"15\" onchange=\"form.azimuth.selectedIndex = 0;\" />\n\
</td>\n\
</tr>\n\
<tr>\n\
<td>\n\
<a href=\"/yoursky/help/horcontrols.html#FieldOfView\">Field of view</a>:\n\
</td>\n\
<td>\n\
<input type=\"text\" name=\"fov\" value=\"%s\" size=\"10\" />\n\
</td>\n\
</tr>\n\
</table>\n\
\n\
",          editazi(horazi),
            eddeg(telfov));
        } else {
            printf("\
\n\
</td>\n\
</tr>\n\
<tr>\n\
<th>\n\
          <a href=\"/yoursky/help/telcontrols.html#AimPoint\">Aim Point</a>\n\
</th>\n\
<td>\n\
<table cellpadding=\"3\">\n\
<tr>\n\
<td>\n\
<a href=\"/yoursky/help/telcontrols.html#RightAscension\">Right Ascension</a>:\n\
</td>\n\
<td>\n\
<input type=\"text\" name=\"lon\" value=\"%s\" size=\"15\" />\n\
</td>\n\
</tr>\n\
<tr>\n\
<td>\n\
<a href=\"/yoursky/help/telcontrols.html#Declination\">Declination</a>:\n\
</td>\n\
<td>\n\
<input type=\"text\" name=\"lat\" value=\"%s\" size=\"15\" />\n\
</td>\n\
<td>\n\
<label><input type=\"radio\" name=\"ns\" %svalue=\"North\" /> North</label>\n\
<label><input type=\"radio\" name=\"ns\" %svalue=\"South\" /> South</label>\n\
</td>\n\
</tr>\n\
\n\
<tr>\n\
<td>\n\
<a href=\"/yoursky/help/telcontrols.html#FieldOfView\">Field of view</a>:\n\
</td>\n\
<td>\n\
<input type=\"text\" name=\"fov\" value=\"%s\" size=\"10\" />\n\
</td>\n\
</tr>\n\
<tr>\n\
<td align=\"center\">\n\
<a href=\"/yoursky/catalogues/catalogues.html\"><b>Find object in catalogue</b></a>\n\
</td>\n\
</tr>\n\
\n\
</table>\n\
\n\
",          editRA(vlon),
            eddeg(abs(vlat)), Chk(vlat >= 0), Chk(vlat < 0),
            eddeg(telfov));
        }

        if (skyMode == paintHOR) {
            printf("\
\n\
</td>\n\
</tr>\n\
<tr>\n\
<th>\n\
          <a href=\"/yoursky/help/horcontrols.html#Site\">Observing Site</a>\n\
</th>\n\
<td>\n\
<table cellpadding=\"3\">\n\
<tr>\n\
<td>\n\
Latitude:\n\
</td>\n\
<td>\n\
<input type=\"text\" name=\"lat\" value=\"%s\" size=\"10\" />\n\
</td>\n\
<td>\n\
<label><input type=\"radio\" name=\"ns\" %svalue=\"North\" /> North</label>\n\
<label><input type=\"radio\" name=\"ns\" %svalue=\"South\" /> South</label>\n\
</td>\n\
</tr>\n\
<tr>\n\
<td>\n\
Longitude:\n\
</td>\n\
<td>\n\
<input type=\"text\" name=\"lon\" value=\"%s\" size=\"10\" />\n\
</td>\n\
<td>\n\
<label><input type=\"radio\" name=\"ew\" %svalue=\"East\" /> East</label>\n\
<label><input type=\"radio\" name=\"ew\" %svalue=\"West\" /> West</label>\n\
</td>\n\
</tr>\n\
\n\
<tr>\n\
<td align=\"center\">\n\
<a href=\"/yoursky/cities.html\"><b>Set for nearby city</b></a>\n\
</td>\n\
</tr>\n\
\n\
</table>\n\
\n\
",          eddeg(abs(siteLat)), Chk(siteLat > 0), Chk(siteLat < 0),
            eddeg(abs(siteLon)), Chk(siteLon < 0), Chk(siteLon > 0));
        }

        printf(TH("\
</td>\n\
</tr>\n\
<tr>\n\
     <th><a href=\"/yoursky/help/telcontrols.html#Options\">Display Options</a>\n\
     </th>\n\
<td>\n\
<input type=\"checkbox\" %sname=\"coords\" /> <a href=\"/yoursky/help/telcontrols.html#Coords\">Ecliptic and equator</a>\n\
\n\
<br />\n\
<input type=\"checkbox\" %sname=\"moonp\" /> <a href=\"/yoursky/help/telcontrols.html#Planets\">Moon and planets</a>\n\
\n\
<br />\n\
<input type=\"checkbox\" %sname=\"deep\" />  <a href=\"/yoursky/help/telcontrols.html#DeepSky\">Deep sky objects of magnitude</a>\n\
 <input type=\"text\" name=\"deepm\" value=\"%.1f\" size=\"3\" /> and brighter\n\
\n\
<br />\n\
 <a href=\"/yoursky/help/telcontrols.html#Constellations\">Constellations:</a>\n\
<br />\n\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<input type=\"checkbox\" %sname=\"consto\" /> <a href=\"/yoursky/help/telcontrols.html#ConstO\">Outlines</a>\n\
<br />\n\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<input type=\"checkbox\" %sname=\"constn\" /> <a href=\"/yoursky/help/telcontrols.html#ConstN\">Names</a>\n\
<label><input type=\"checkbox\" %sname=\"consts\" /> abbreviate?</label>\n\
<br />\n\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<input type=\"checkbox\" %sname=\"constb\" /> <a href=\"/yoursky/help/telcontrols.html#ConstB\">Boundaries</a>\n\
\n\
<br />\n\
<a href=\"/yoursky/help/telcontrols.html#Stars\">Stars:</a>\n\
<br />\n\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href=\"/yoursky/help/telcontrols.html#StarMag\">Show stars brighter than magnitude</a> <input type=\"text\" name=\"limag\" value=\"%.1f\" size=\"5\" />\n\
<br />\n\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<input type=\"checkbox\" %sname=\"starn\" /> <a href=\"/yoursky/help/telcontrols.html#StarName\">Names for magnitude</a>\n\
 <input type=\"text\" name=\"starnm\" value=\"%.1f\" size=\"5\" /> and brighter\n\
<br />\n\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<input type=\"checkbox\" %sname=\"starb\" /> <a href=\"/yoursky/help/telcontrols.html#StarBFlam\">Bayer/Flamsteed codes</a> for mag.\n\
 <input type=\"text\" name=\"starbm\" value=\"%.1f\" size=\"5\" /> and brighter\n\
<br />\n\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<input type=\"checkbox\" %sname=\"showm\" /> <a href=\"/yoursky/help/telcontrols.html#StarShowM\">Show magnitudes</a> between\n\
 <input type=\"text\" name=\"showmb\" value=\"%.1f\" size=\"5\" /> and <input type=\"text\" name=\"showmd\" value=\"%.1f\" size=\"5\" />\n\
\n\
"),     Chk(skyShowCoords), Chk(skyShowPlanets), Chk(skyShowDeep), skyDeepMag,
        Chk(skyShowConstellations), Chk(skyShowConnames), Chk(skyAbbreviateConnames), Chk(skyShowConbounds),
        skyLimitMag,
        Chk(skyShowName), skyNameMag, Chk(skyShowBflam), skyBflamMag,
        Chk(telShowMag), telShowMagMax, telShowMagMin);

        if (skyMode == paintHOR) {
            printf("\
<br /><input type=\"checkbox\" %sname=\"terrain\" /> <a href=\"/yoursky/help/horcontrols.html#Terrain\">Terrain at horizon</a> &nbsp;&nbsp; <a href=\"/yoursky/help/horcontrols.html#Terrough\">Roughness:</a> \n\
            <input type=\"text\" name=\"terrough\" value=\"%.1f\" size=\"5\" />\n\
            &nbsp;&nbsp; <input type=\"checkbox\" %sname=\"scenery\" /> <a href=\"/yoursky/help/horcontrols.html#Scenery\"> with scenery</a>\n\
",          Chk(horShowTerrain), horSceneryRoughness, Chk(horShowScenery));
        } else {
            printf("\
<br /><input type=\"checkbox\" %sname=\"flip\" /> <a href=\"/yoursky/help/telcontrols.html#Invert\">Invert North and South</a>\n\
",          Chk(flipNorthSouth));
        }

        printf(TH("\
\n\
<br /><a href=\"/yoursky/help/telcontrols.html#Imgsize\">Image size</a>: <input type=\"text\" name=\"imgsize\" value=\"%d\" size=\"6\" /> pixels\n\
<br /><a href=\"/yoursky/help/telcontrols.html#Fontscale\">Font scale</a>: <input type=\"text\" name=\"fontscale\" value=\"%.1f\" size=\"4\" />\n\
\n\
\n\
<br /><a href=\"/yoursky/help/telcontrols.html#Scheme\">Colour scheme</a>: <select name=\"scheme\" size=\"1\">\n\
<option value=\"0\"%s>Colour</option>\n\
<option value=\"1\"%s>Black on white background</option>\n\
<option value=\"2\"%s>White on black background</option>\n\
<option value=\"3\"%s>Night vision (red)</option>\n\
</select>\n\
</td>\n\
</tr>\n\
\n\
<tr>\n\
   <th><a href=\"/yoursky/help/elements.html\">Asteroid and<br />Comet Tracking</a>\n\
</th>\n\
<td align=\"center\">\n\
<br />\n\
"),     tmsize,  skyFontScale,
        Sel(skyColourScheme == 0), Sel(skyColourScheme == 1),
        Sel(skyColourScheme == 2), Sel(skyColourScheme == 3));

    /* Include elements for object being tracked, if any */

    if (aTracked) {
        L("Orbital elements for asteroid or comet", "elements");
        for (j = (maxOrbitalElements - 1); j >= 0; j--) {
            if (strlen(orbitalElements[j]) > 0) {
                break;
            }
        }
        j = max(1, j + 1);
    } else {
        W("Paste <a href=\"/yoursky/help/elements.html\">orbital elements</a> below:");
        j = 1;
    }
    printf("  <label><input type=\"checkbox\" %sname=\"edump\" value=\"-xe\" /> Echo elements</label>\n",
        Chk(dumpelem));
    W("<br />");
    printf("<textarea name=\"elements\" rows=\"%d\" cols=\"40\">", j);
    for (i = 0; i < j; i++) {
#ifdef OLDWAY
        W(orbitalElements[i]);
#else
        write_html_content(stdout, orbitalElements[i]);
        putchar('\n');
#endif
    }

    printf("\
</textarea><br />\n\
</td>\n\
</tr>\n\
</table>\n\
\n\
</center>\n\
</form>\n\
\n\
");

    /* Append ephemeris after the control panel. */

    W("<p />");
                   L("Ephemeris", "ephemeris");
    /* If this is a horizon view, we know the observing site and
       generate the ephemeris with updatePlanetComplete.  Otherwise,
       we generate only a geocentric ephemeris. */
    if (skyMode == paintHOR) {
        updatePlanetComplete(jt, stdout, edvpos(vlat, vlon), qelt);
    } else {
        updatePlanet(jt, stdout, edvpos(vlat, vlon), qelt);
    }

    if (dumpelem && aTracked) {
        print_elements(stdout);
    }
#undef W
#undef L
}

/* Main program. */

int main(int argc, char *argv[])
{
    double vlat, vlon;
    int i, f = 0,
        tmsize = 512,
#ifdef PPM_SUPPORT
        wppm = FALSE,
#endif
        trackelem = FALSE, html = FALSE,
        skycolour, aimAt = -1, endopts = FALSE,
        dynimg = FALSE, stateless = FALSE;
    char *di, *cp, opt, *qelt = NULL, *cachep;
    char cacheName[PATH_MAX];
    char tbuf[132];
    long cctime;
    FILE *ofile = stdout;
    static char *mname[12] = {
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
        "Nov", "Dec" };

#ifdef DBLOG
    db = fopen("/tmp/dblog", "w");
#endif
    progpath = argv[0];               /* Save path used to call us */

#ifdef TimeLimit
    sigset(SIGALRM, (void (*)()) dingaling);
    alarm(TimeLimit);
#endif

    /*  Clear orbitalElements array.  */
    for (i = 0; i < maxOrbitalElements; i++) {
        orbitalElements[i][0] = 0;
    }

    /*  If the WWW_di environment variable is defined, we've been invoked
        from a "stateless" mode document to generate a dynamic image with the
        arguments packed into the argument block.  Unpack the block into a
        synthesised argc/argv and transcribe the orbital elements (if any) to
        the orbitalElements array.  Existence of a WWW_di block overrides any
        arguments specified on the command line.  */

    di = getenv("QUERY_STRING");
    if ((di != NULL) && (strncmp(di, "di=", 3) == 0)) {
        unsigned char *ar = (unsigned char *) unpackargs(di + 3);
        int i, ne;
        char *cp;

        di += 3;
#ifdef ZZZ
        int na;

        fprintf(stderr, "IV = %02X  Version: %d\n", ar[0], ar[1]);
        na = ar[2];
        fprintf(stderr, "Arguments: %d\n", na);
        cp = (char *) (ar + 3);
        for (i = 1; i < na; i++) {
            fprintf(stderr, "   %d:  \"%s\"\n", i, cp);
            cp += strlen(cp) + 1;
        }

        ne = *((unsigned char *) cp);
        cp++;

        fprintf(stderr, "Elements: %d\n", ne);
        for (i = 0; i < ne; i++) {
            fprintf(stderr, "   %d:  \"%s\"\n", i, cp);
            cp += strlen(cp) + 1;
        }
return 0;
#endif
/* fprintf(stderr, "DI block: %s\n", di); */

        if (ar[1] != 1) {
            fprintf(stderr, "%s: Invalid version %d in WWW_di option block.\n", progpath, ar[1]);
            abort();
        }
        argc = ar[2];
        argv = malloc(sizeof(char *) * argc);
        if (argv == NULL) {
            fprintf(stderr, "%s: Unable to allocate %d item argument vector for WWW_di option block.\n",
                progpath, argc);
            abort();
        }
        argv[0] = progpath;

        /*  Link arguments in block to new argv pointer table.  */

        cp = (char *) (ar + 3);
        for (i = 1; i < argc; i++) {
            argv[i] = cp;
            cp += strlen(cp) + 1;
        }

        /*  Transcribe elements (if any) to orbitalElements array.  */

        ne = *((unsigned char *) cp);
        cp++;
        for (i = 0; i < ne; i++) {
            if (i < maxOrbitalElements) {
                strncpy(savedOrbitalElements[i], cp, 132);
                savedOrbitalElements[i][131] = 0;           /* Just in case */
            }
            cp += strlen(cp) + 1;
        }
        embeddedImage = TRUE;
    } else {
        di = NULL;
    }


    /*  If we are called with no command line arguments and no
        dynamic image parameter block was supplied, this is a
        direct invocation which expects us to decode the CGI
        arguments ourself.  We call the embedded uncgi()
        function to unpack them into environment variables
        and then synthesise a command line with options set from them.  */

    if ((di == NULL) && (argc == 1)) {
        char *arg, *arg2;
#define MaxArgs 48
        static char *orgv[MaxArgs + 1];
        int orgc = 1;
        int needuncgi = TRUE, i, zedMode = FALSE;

        /* We may have been invoked with a URL which embeds a call
           on uncgi, since people have copied these URLs created
           by the custom request page into their own pages over the
           years.  If so, we don't want to call uncgi() again and mess
           up the already-built environment.  We detect this case by
           scanning the environment to see if it already contains a
           variable which begins with "WWW_".  If so, we skip our
           own call on the uncgi() function. */

        for (i = 0; environ[i] != NULL; i++) {
            if (strncmp(environ[i], "WWW_", 4) == 0) {
                needuncgi = FALSE;
                break;
            }
        }

        if (needuncgi) {
            uncgi();
        }

        directCall = TRUE;

#define BoolOpt(name, option)                           \
        if (((arg = getenv("WWW_" name)) != NULL)) {    \
            orgv[orgc++] = "-" option "1";              \
        } else {                                        \
            orgv[orgc++] = "-" option "0";              \
        }

#define MagOpt(name, option)                                    \
        if ((arg = getenv("WWW_" name)) != NULL) {              \
            char *s = malloc(strlen(arg) + strlen(option) + 2); \
            strcpy(s, "-" option);                              \
            strcat(s, arg);                                     \
            orgv[orgc++] = s;                                   \
        }

        orgv[0] = argv[0];
        orgv[orgc++] = "-l";
        orgv[orgc++] = "-t";

        /* Set horizon option if invoked as "Yourhorizon" */
        if (strstr(progpath, "Yourhorizon") != NULL) {
            orgv[orgc++] = "-y";
            skyMode = paintHOR;             /* Preset horizon mode for convenience in subsequent code */

            /* Show terrain ? */
            if (getenv("WWW_terrain") != NULL) {
                orgv[orgc++] = "-mt1";
            } else {
                orgv[orgc++] = "-mt0";
            }

            /* Scenery for terrain */
            if (getenv("WWW_scenery") != NULL) {
                orgv[orgc++] = "-ms1";
            } else {
                orgv[orgc++] = "-ms0";
            }

            MagOpt("terrough", "mr");           /* Terrain roughness */

            /* Express option setting to reduce size of files using defaults */

            if (getenv("WWW_z") != NULL) {
                zedMode = TRUE;
                orgv[orgc++] = "-e0";
                orgv[orgc++] = "-i512";
                orgv[orgc++] = "-k1";
                orgv[orgc++] = "-p1";
                orgv[orgc++] = "-ca0";
                orgv[orgc++] = "-cb1";
                orgv[orgc++] = "-cn1";
                orgv[orgc++] = "-co1";
                orgv[orgc++] = "-ds1";
                orgv[orgc++] = "-sb1";
                orgv[orgc++] = "-sn1";
                orgv[orgc++] = "-ss0";
                orgv[orgc++] = "-mt1";
                orgv[orgc++] = "-ms1";
                orgv[orgc++] = "-dm3.0";
                orgv[orgc++] = "-sm5.5";
                orgv[orgc++] = "-so3";
                orgv[orgc++] = "-sc3.5";
            }
        } else {

            /* This is a virtual telescope image */

            /* Aim at solar system object */
            if ((arg = getenv("WWW_aim")) != NULL) {
                char *s = malloc(strlen(arg) + 3);
                strcpy(s, "-a");
                strcpy(s + 2, arg);
                orgv[orgc++] = s;
            }


            BoolOpt("flip", "f");   /* Flip north and south from default ? */

            /* Express option setting to reduce the size of the cities file */

            if ((arg = getenv("WWW_z")) != NULL) {
                zedMode = TRUE;
                orgv[orgc++] = "-e0";
                orgv[orgc++] = "-i512";
                orgv[orgc++] = "-k1";
                orgv[orgc++] = "-p1";
                orgv[orgc++] = "-ca0";
                orgv[orgc++] = "-cb1";
                orgv[orgc++] = "-cn1";
                orgv[orgc++] = "-co1";
                orgv[orgc++] = "-ds1";
                orgv[orgc++] = "-sb1";
                orgv[orgc++] = "-sn1";
                orgv[orgc++] = "-ss0";
                if (strcmp(arg, "2") == 0) {
                   orgv[orgc++] = "-dm5";
                   orgv[orgc++] = "-sm6";
                   orgv[orgc++] = "-so3";
                   orgv[orgc++] = "-sc5";
                }
            }
        }

        /* Dynamic image request */
        if (getenv("WWW_dynimg") != NULL) {
            orgv[orgc++] = "-j";
        } else {
            orgv[orgc++] = "-h";
        }

        if (!zedMode) {

            /* Set image size */
            if ((arg = getenv("WWW_imgsize")) != NULL) {
                char *s = malloc(strlen(arg) + 3);
                strcpy(s, "-i");
                strcpy(s + 2, arg);
                orgv[orgc++] = s;
            }

            /* Date and time */
            if ((arg = getenv("WWW_date")) != NULL) {
                switch (arg[0]) {
                    case '0':           /* Now */
                    default:
                        orgv[orgc++] = "-e0";
                        break;

                    case '1':           /* UTC date and time */
                        if ((arg2 = getenv("WWW_utc")) != NULL) {
                            char *s = malloc(strlen(arg2) + 4);
                            strcpy(s, "-e1");
                            strcpy(s + 3, arg2);
                            orgv[orgc++] = s;
                        } else {
                            orgv[orgc++] = "-e0";
                        }
                        break;

                    case '2':           /* Julian day */
                        if ((arg2 = getenv("WWW_jd")) != NULL) {
                            char *s = malloc(strlen(arg2) + 4);
                            strcpy(s, "-e2");
                            strcpy(s + 3, arg2);
                            orgv[orgc++] = s;
                        } else {
                            orgv[orgc++] = "-e0";
                        }
                        break;
                }
            } else {
                orgv[orgc++] = "-e0";   /* Now */
            }

            /* Dump orbital elements option */
            if (((arg = getenv("WWW_edump")) != NULL) &&
                (strcmp(arg, "-xe") == 0)) {
                orgv[orgc++] = "-xe";
            }

            /* Colour scheme */
            if (((arg = getenv("WWW_scheme")) != NULL) &&
                ((arg[0] >= '0') && (arg[0] <= '3'))) {
                char *s = malloc(4);
                strcpy(s, "-g");
                s[2] = arg[0];
                s[3] = EOS;
                orgv[orgc++] = s;
            }

            BoolOpt("coords", "k");             /* Show ecliptic and equator ? */
            BoolOpt("moonp", "p");              /* Show moon and planets ? */
            BoolOpt("consta", "ca");            /* Align constellation names with horizon ? */
            BoolOpt("constb", "cb");            /* Show constellation boundaries ? */
            BoolOpt("constn", "cn");            /* Show constellation names ? */
            BoolOpt("consts", "cs");            /* Abbreviate constellation names ? */
            BoolOpt("consto", "co");            /* Show constellation outlines ? */
            BoolOpt("deep", "ds");              /* Show deep sky objects ? */
            BoolOpt("starb", "sb");             /* Show star Bayer/Flamsteed codes ? */
            BoolOpt("starn", "sn");             /* Show star names ? */
            BoolOpt("showm", "ss");             /* Show star magnitudes ? */

            MagOpt("limag", "sm");              /* Star limiting magnitude */
            MagOpt("starnm", "so");             /* Star name limiting magnitude */
            MagOpt("starbm", "sc");             /* Star Bayer/Flamsteed limiting magnitude */
            MagOpt("showmb", "st");             /* Star magnitude display brightest magnitude */
            MagOpt("showmd", "su");             /* Star magnitude display dimmest magnitude */
            MagOpt("deepm", "dm");              /* Deep space limiting magnitude */
            MagOpt("fontscale", "sy");          /* Font size scale factor */
        }


        /* End of options.  Add terminating "-" and observer's
           latitude and longitude if specified. */

        orgv[orgc++] = "-";

        if (skyMode == paintHOR) {

            /* This is a horizon view */

            /* Observer Latitude */
            if ((arg = getenv("WWW_lat")) != NULL) {
                char *s = malloc(strlen(arg) + 2);
                s[0] = EOS;
                if ((arg2 = getenv("WWW_ns")) != NULL) {
                    if (arg2[0] == 'l') {
                        arg2++;
                    }
                    if (arg2[0] == 'S') {
                        strcpy(s, "-");
                    }
                }
                strcat(s, arg);
                orgv[orgc++] = s;
            } else {
                orgv[orgc++] = "0";
            }

            /* Observer Longitude */
            if ((arg = getenv("WWW_lon")) != NULL) {
                char *s = malloc(strlen(arg) + 2);
                s[0] = 0;
                if (((arg2 = getenv("WWW_ew")) != NULL) &&
                    (arg2[0] != 'W')) {
                    strcpy(s, "-");
                }
                strcat(s, arg);
                orgv[orgc++] = s;
            } else {
                orgv[orgc++] = "0";
            }
        } else {

            /* This is a virtual telescope view */

            /* Object declination */
            if ((arg = getenv("WWW_lat")) != NULL) {
                char *s = malloc(strlen(arg) + 2);
                s[0] = EOS;
                if ((arg2 = getenv("WWW_ns")) != NULL) {
                    if (arg2[0] == 'l') {
                        arg2++;
                    }
                    if (arg2[0] == 'S') {
                        strcpy(s, "-");
                    }
                }
                strcat(s, arg);
                orgv[orgc++] = s;
            } else {
                orgv[orgc++] = "0";
            }

            /* Object right ascension */
            if ((arg = getenv("WWW_lon")) != NULL) {
                orgv[orgc++] = strdup(arg);
            } else {
                orgv[orgc++] = "0";
            }

        }

        /* Field of view */
        if ((arg = getenv("WWW_fov")) != NULL) {
            orgv[orgc++] = strdup(arg);
        } else {
            orgv[orgc++] = "45";
        }

        /* Azimuth of horizon view */
        if (skyMode == paintHOR) {
            if ((arg = getenv("WWW_azimuth")) != NULL) {
                if ((strcmp(arg, "V") == 0) &&
                    ((arg2 =  getenv("WWW_azideg")) != NULL)) {
                    orgv[orgc++] = strdup(arg2);
                } else {
                    orgv[orgc++] = strdup(arg);
                }
            } else {
                if ((arg2 =  getenv("WWW_azideg")) != NULL) {
                    orgv[orgc++] = strdup(arg2);
                } else {
                    orgv[orgc++] = "0";
                }
            }
        }


        orgv[orgc] = NULL;

        if (orgc >= MaxArgs) {
            fprintf(stderr, "%s: Too many command line arguments (%d generated, %d max)\n",
                progpath, orgc, MaxArgs);
            abort();
        }
#undef MaxArgs

        for (i = 1; i < orgc; i++) {
            if (orgv[i] == NULL) {
                fprintf(stderr, "%s: Allocation of command line argument %d failed\n",
                    progpath, i);
                abort();
            }
        }

        argv = orgv;
        argc = orgc;

        /* If comet or asteroid orbital elements were specified, save
           a pointer to them for subsequent parsing. */
        if ((arg = getenv("WWW_elements")) != NULL) {
            /* If the last line of the elements is not terminated by
               a line feed, add one so alement_rs() in astrelem.c
               doesn't get confused. */
            if (arg[strlen(arg) - 1] != '\n') {
                char *s = malloc(strlen(arg) + 2);
                strcpy(s, arg);
                strcat(s, "\n");
                arg = s;
            }
            elementSpecification = arg;
        }

    }

    /*  Process command line options.  */

    for (i = 1; i < argc; i++) {
        char sopt;

        cp = argv[i];
        if (strcmp(cp, "-") == 0) {   /* Bare "-" marks end of options, start of arguments */
            endopts = TRUE;
            continue;
        }
        if ((!endopts) && (*cp == '-' && !isdigit(cp[1]))) {
            opt = *(++cp);
            if (islower(opt)) {
                opt = toupper(opt);
            }
            sopt = cp[1];
            if (islower(sopt)) {
                sopt = toupper(cp[1]);
            }
            switch (opt) {

                case 'A':             /* Aim at solar system object */
                    aimAt = atoi(cp + 1);
                    if (aimAt < 0 || aimAt > 10) {
                        aimAt = -1;
                    }
                    break;

                case 'C':             /* Constellations */
                    switch (sopt) {
                        case 'B':     /* -CB1/0  --  Boundaries ? */
                            skyShowConbounds = atoi(cp + 2);
                            break;

                        case 'O':     /* -CO1/0  --  Outlines ? */
                            skyShowConstellations = atoi(cp + 2);
                            break;

                        case 'N':     /* -CN1/0  --  Names ? */
                            skyShowConnames = atoi(cp + 2);
                            break;

                        case 'S':     /* -CS1/0  --  Abbreviate names ? */
                            skyAbbreviateConnames = atoi(cp + 2);
                            break;
                    }
                    break;

                case 'D':             /* Deep Space */
                    switch (sopt) {
                        case 'M':     /* -DMmag  --  Limiting magnitude */
                            skyDeepMag = atof(cp + 2);
                            break;

                        case 'S':     /* -DS1/0  --  Show deep space objects ? */
                            skyShowDeep = atoi(cp + 2);
                            break;
                    }
                    break;

                case 'E':             /* Epoch */
                    switch (cp[1]) {
                        case '0':
                            dto = 0;
                            break;

                        case '1':
                            {
                                char del;

                                dto = 1;
                                ctim.tm_year = -5000;
                                ctim.tm_mon = ctim.tm_hour =
                                    ctim.tm_min = ctim.tm_sec = 0;
                                ctim.tm_mday = 1;
                                sscanf(cp + 2, "%d%c%d%c%d %d%c%d%c%d",
                                    &ctim.tm_year, &del, &ctim.tm_mon, &del, &ctim.tm_mday,
                                    &ctim.tm_hour, &del, &ctim.tm_min, &del, &ctim.tm_sec);
                                if (ctim.tm_year < -4712 || ctim.tm_year >= 8000 ||
                                    ctim.tm_mon < 1 || ctim.tm_mon > 12 ||
                                    ctim.tm_mday < 1 || ctim.tm_mday > 31 ||
                                    ctim.tm_hour < 0 || ctim.tm_hour >= 24 ||
                                    ctim.tm_min < 0 || ctim.tm_min >= 60 ||
                                    ctim.tm_sec < 0 || ctim.tm_sec >= 60) {
                                    dto = 0;
                                } else {
                                    ctim.tm_year -= 1900;
                                    ctim.tm_mon--;
#ifndef Solaris
                                    ctim.tm_gmtoff = 0;
                                    ctim.tm_zone = "UTC";
#endif
                                    ctim.tm_isdst = 0;
                                    jt = jtime(&ctim);
                                    ctim.tm_wday = fmod(jt + 1.5, 7);
                                }
                                break;
                            }

                        case '2':
                            {   long a;
                                double ejt;

                                dto = 2;
                                jt = atof(cp + 2);
                                if (jt < 0.5 || jt > VSOP87_INVALID) {
                                    dto = 0;
                                } else {
                                    ejt = jt - 0.5;
                                    jyear(jt, &a, &ctim.tm_mon, &ctim.tm_mday);
                                    ctim.tm_wday = fmod(jt + 1.5, 7);
                                    ctim.tm_year = a - 1900;
                                    ctim.tm_mon--;
#ifndef Solaris
                                    ctim.tm_gmtoff = 0;
                                    ctim.tm_zone = "UTC";
#endif
                                    ctim.tm_isdst = 0;
                                    a = (long) (((ejt - ((long) ejt)) * 86400.0) + 0.5);
                                    ctim.tm_hour = a / 3600;
                                    ctim.tm_min = (a / 60) % 60;
                                    ctim.tm_sec = (a % 60);
                                }
                            }
                            break;
                    }
                    break;

                case 'F':             /* -F0/1  --  Flip north and south */
                    flipNorthSouth = atoi(cp + 1);
                    break;

                case 'G':             /* -G[0-3]  --  Colour scheme  */
                    skyColourScheme = atoi(cp + 1);
                    if (skyColourScheme < 0 || skyColourScheme > 3) {
                        skyColourScheme = 0;
                    }
                    break;

                case 'H':             /* Generate HTML output */
                    html = TRUE;
                    break;

                case 'I':             /* Map image size */
                    tmsize = atoi(cp + 1);
                    tmsize = max(100, min(tmsize, MAX_IMAGE_SIZE));
                    break;

                case 'J':             /* Generate dynamic image result */
                    dynimg = TRUE;
                    break;

                case 'K':             /* Show coordinates (ecliptic and equator) ? */
                    skyShowCoords = atoi(cp + 1);
                    break;

                case 'L':
                    stateless = TRUE; /* Use stateless image protocol */
                    break;

                case 'M':             /* Terrain-related options for horizon views */
                    switch (sopt) {
                        case 'R':     /* -MRnum  --  Roughness: fractal dimension of terrain */
                            horSceneryRoughness = atof(cp + 2);
                            horSceneryRoughness = min(2, max(horSceneryRoughness, -100));
                            break;

                        case 'S':     /* -MS1/0  --  Add scenery to terrain ? */
                            horShowScenery = atoi(cp + 2);
                            break;

                        case 'T':     /* -MT1/0  --  Draw terrain at horizon ? */
                            horShowTerrain = atoi(cp + 2);
                            break;
                    }
                    break;


                case 'O':             /* Output .GIF file */
                    cachep = strstr(cp + 1, "/cache");
                    assert(cachep != NULL);
                    strcpy(cacheName, cachep);
                    ofile = fopen(cp + 1, "w");
                    if (ofile == NULL) {
                        ofile = stdout;
                    }
                    break;

                case 'P':             /* -P1/0  --  Show Moon and planets */
                    skyShowPlanets = atoi(cp + 1);
                    break;

                case 'S':             /* Star-related options */
                    switch (sopt) {
                        case 'B':     /* -SB1/0  --  Show Bayer/Flamsteed codes */
                            skyShowBflam = atoi(cp + 2);
                            break;

                        case 'C':     /* -SCmag  --  Bayer/Flamsteed limiting magnitude */
                            skyBflamMag = atof(cp + 2);
                            break;

                        case 'M':     /* -SMmag  --  Limiting magnitude */
                            skyLimitMag = atof(cp + 2);
                            break;

                        case 'N':     /* -SN1/0  --  Star names ? */
                            skyShowName = atoi(cp + 2);
                            break;

                        case 'O':     /* -SOmag  --  Star name limiting magnitude */
                            skyNameMag = atof(cp + 2);
                            break;

                        case 'S':     /* -SS1/0  --  Show magnitudes ? */
                            telShowMag = atoi(cp + 2);
                            break;

                        case 'T':     /* -STmag  --  Show magnitude brightest (minimum numerically) */
                            telShowMagMax = atof(cp + 2);
                            break;

                        case 'U':     /* -SUmag  --  Show magnitude dimmest (maximum numerically) */
                            telShowMagMin = atof(cp + 2);
                            break;

                        case 'Y':     /* -SYfac  --  Text label font scale factor  */
                            skyFontScale = max(0.1, min(atof(cp + 2), 3.0));
                            break;
                    }
                    break;

                case 'T':             /* Track asteroid or comet */
                    trackelem = TRUE;
                    break;

#ifdef PPM_SUPPORT
                case 'W':             /* Write PPM to standard output */
                    wppm = TRUE;
                    break;
#endif

                case 'X':             /* Diagnostic or extended output */
                    switch (sopt) {
                        case 'E':
                            dumpelem = TRUE;
                            break;

#ifdef SAVESET
                        case 'H':
                            gotten = TRUE;
                            break;

                        case 'S':
                            bookmark = TRUE;
                            break;
#endif
                    }
                    break;

                case 'Y':           /* Paint horizon view */
                    skyMode = paintHOR;
                    break;

                case '?':
                case 'U':
    fprintf(stderr, "Options:");

    fprintf(stderr,"\n    -aObject Aim telescope at solar system Object");

    fprintf(stderr,"\n    -cb[10] Show constellation boundaries?");
    fprintf(stderr,"\n    -co[10] Show constellation outlines?");
    fprintf(stderr,"\n    -cn[10] Show constellation names?");

    fprintf(stderr,"\n    -dmN    Deep sky limiting magnitude N");
    fprintf(stderr,"\n    -ds[10] Show deep sky objects?");

    fprintf(stderr,"\n    -e0     Epoch = Now");
    fprintf(stderr,"\n    -e1dt   Epoch = Date/time in UTC");
    fprintf(stderr,"\n    -e2jd   Epoch = Julian date jd");

    fprintf(stderr,"\n    -f[10]  Flip north and south ?");

    fprintf(stderr,"\n    -gN     Colour scheme N 0=colour, 1=black/white 2=white/black 3=night vision");

    fprintf(stderr,"\n    -h      Create HTML file");

    fprintf(stderr,"\n    -innn   Use image size of nnn pixels");

    fprintf(stderr,"\n    -j      Generate dynamic image");

    fprintf(stderr,"\n    -k[10]  Show ecliptic and equator co-ordinates?");

    fprintf(stderr,"\n    -l      Use stateless image protocol");

    fprintf(stderr,"\n    -mrN    Set terrain roughness fractal dimension to N");
    fprintf(stderr,"\n    -ms[10] Add scenery to terrain?");
    fprintf(stderr,"\n    -mt[10] Draw terrain at horizon?");

    fprintf(stderr,"\n    -ofname GIF output to fname");

    fprintf(stderr,"\n    -p[10]  Show moon and planets");

    fprintf(stderr,"\n    -sb[10] Show Bayer/Flamsteed codes?");
    fprintf(stderr,"\n    -scN    Bayer/Flamsteed limiting magnitude N");
    fprintf(stderr,"\n    -smN    Star limiting magnitude N");
    fprintf(stderr,"\n    -sn[10] Show star names?");
    fprintf(stderr,"\n    -soN    Star name limiting magnitude N");
    fprintf(stderr,"\n    -ss[10] Show star magnitudes?");
    fprintf(stderr,"\n    -stN    Show magnitude brightest (minimum numerically) N");
    fprintf(stderr,"\n    -suN    Show magnitude dimmest (maximum numerically) N");

    fprintf(stderr,"\n    -t      Track asteroid or comet");

    fprintf(stderr,"\n    -u      Print this message");
#ifdef PPM_SUPPORT
    fprintf(stderr,"\n    -w      Write PPM, not GIF output");
#endif
    fprintf(stderr,"\n    -xe     Echo asteroid/comet elements");
#ifdef SAVESET
    fprintf(stderr,"\n    -xh     Use action=get for form to save parameters");
    fprintf(stderr,"\n    -xs     Save settings for bookmark");
#endif
    fprintf(stderr,"\n    -y      Generate horizon view");
    fprintf(stderr,"\n");
                    return 0;
            }
        } else {
            if (isdigit(cp[0]) || cp[0] == '+' || cp[0] == '-') {
                switch (f) {
                    case 0:
                        if (skyMode == paintHOR) {
                            siteLat = parseDeg(cp);
                        } else {
                            teldec = parseDeg(cp);
                        }
                        f++;
                        break;

                    case 1:
                        if (skyMode == paintHOR) {
                            siteLon = parseDeg(cp);
                        } else {
                            telra = parseRA(cp);
                        }
                        f++;
                        break;

                    case 2:
                        telfov = parseDeg(cp);
                        f++;
                        break;

                    case 3:
                        if (skyMode == paintHOR) {
                            horazi = parseDeg(cp);
                        } else {
                            fprintf(stderr, "Extra coordinates ignored.\n");
                        }
                        break;

                    default:
                        fprintf(stderr, "Extra coordinates ignored.\n");
                        break;
                }
            }
        }
    }

    /*  If we received a WWW_di argument block, adjust the options accordingly
        to generate the dynamic image.  This is a big kludge and should be looked
        at closely when options are added or modified.   */

    if (di) {
        stateless = FALSE;
        html = FALSE;
        dynimg = TRUE;
    }


    if (dto == 0) {
        time(&cctime);
        ctim = *gmtime(&cctime);
        jt = jtime(&ctim);
    }
    sunpos(jt, TRUE, &sunra, &sundec, &earthrv, &sunlong);
    sprintf(dtutc, "%d-%02d-%02d %d:%02d:%02d",
        ctim.tm_year + 1900, ctim.tm_mon + 1, ctim.tm_mday,
        ctim.tm_hour, ctim.tm_min, ctim.tm_sec);

    /* Clear asteroid/comet orbital elements . */

    for (i = 0; i < maxOrbitalElements; i++) {
        orbitalElements[i][0] = 0;
    }

    /* Load icon bitmap. */

    load_bitmap(locfile(Cscheme("yourtel-icons.bmp", "yourtel-icons-b.bmp", "yourtel-icons-w.bmp", "yourtel-icons-r.bmp")));
    assert(bmBits != NULL);

    /* Find the colour table indices of colours we're going to use in
       drawing in the bitmap.  Note that we can reserve any needed
       colours which aren't used in the icons by including a dummy
       icon in the icon bitmap which references all the additional
       required colours. */

#define findColour(cindex, r, g, b)  if (bmi.bmiColors[i].rgbRed == r &&  \
                                         bmi.bmiColors[i].rgbGreen == g && \
                                          bmi.bmiColors[i].rgbBlue == b) {  \
/*fprintf(stderr, "Index of %d,%d,%d is %d\n", r, g, b, i);*/ \
                                          cindex = i; }
    for (i = 0; i < bmi.bmiHeader.biClrUsed; i++) {
        findColour(transparent, 255, 0, 255);
        findColour(cBlack, 0, 0, 0);
        findColour(cBlue, 0, 0, 255);
        findColour(cGreen, 0, 255, 0);
        findColour(cRed, 255, 0, 0);
        findColour(cBorder, 182, 182, 182);
        findColour(cWhite, 255, 255, 255);
        findColour(cDkCyan, 0, 128, 128);
        findColour(cDkRed, 128, 0, 0);
        findColour(cYellow, 255, 255, 0);
        findColour(cGrey, 128, 128, 128);
        findColour(cLtGrey, 192, 192, 192);
        findColour(cDkGreen, 0, 128, 0);
        findColour(cPink, 255, 128, 128);
        findColour(cDkBlue, 0, 0, 128);
    }

    /* Calculate position of comet or asteroid from elements, if any. */

    if (trackelem) {
        astrelem(di != NULL);
    }

    /* Consistency and sanity checks on arguments. */

    if (skyMode == paintHOR) {
        if (telfov > 90) {
            telfov = 90;
        } else if (telfov < 15) {
            telfov = 15;
        }
    } else {
        if (telfov > 90) {
            telfov = 90;
        }
    }

    /* Calculate the position of the Moon and planets if
       the date is within the validity of VSOP87. */

    if (jt < VSOP87_INVALID) {
        calcPlanet(jt);
    } else {
        planet_info[0].alt = -90;     /* Force sky colour black */
    }

    /* If we're aiming at a solar system object, calculate its
       position and plug into the aim point. */

    if ((jt >= VSOP87_INVALID) || (aimAt == 10 && !aTracked)) {
        aimAt = -1;
    }
    if (aimAt >= 0) {
        teldec = planet_info[aimAt].dec;
        telra = planet_info[aimAt].ra;
    }

    if (skyMode == paintHOR) {
        vlat = dtr(siteLat);
        vlon = dtr(siteLon);
        sprintf(viewdesc, "View toward horizon from %s, azimuth %s",
            edvpos(vlat, vlon), editazi(horazi));
        strcpy(viewdesch, viewdesc);
    } else {
        vlat = teldec;
        vlon = telra;
        sprintf(viewdesc, "Aim point RA: %s Dec: %s", editRA(vlon), eddec(vlat));
        sprintf(viewdesch, "Aim point RA: %s Dec: %s", editRA(vlon), eddech(vlat));
    }

    /*  Generate HTTP response header.  */

    if (html) {
        printf("Content-type: text/html\r\n");
        printf("\r\n");
    } else {
        if (dynimg) {
            char *ruri = getenv("REQUEST_URI");

#ifdef PPM_SUPPORT
            if (wppm) {
                printf("Content-type: image/x-portable-pixmap\r\n");
            } else
#endif
            {
                printf("Content-type: image/gif\r\n");
            }
            if (ruri != NULL) {
                printf("Content-Location: %s\r\n", ruri);
            }
            printf("Pragma: no-cache\r\n");
            /* printf("Cache-Control: no-cache\r\n");
               *** "Cache-Control: pricate" added by server for all cgi-bin
                   requests at Fourmilab. *** */
            printf("\r\n");
        }
        /*  If dynimg is not set, output is simply the image, written
            to standard output, with no HTTP response header.  This is
            used primarily for low-level debugging of image generation
            without the need to get the whole Web pipeline in the act. */
    }

    /*  If this is not a stateless request, generate the image.  */

    if (dynimg || (!stateless)) {
        /* Create the image. */

        if (skyMode == paintHOR) {

        /* Determine sky and legend colour based on altitude of Sun
           and selected colour scheme. */

            skycolour = (planet_info[0].alt < -18.0) ?
                                          Cscheme(cBlack, cWhite, cBlack, cBlack) :          /* Night */
                        ((planet_info[0].alt > -1) ? Cscheme(cBlue, cWhite, cBlack, cBlack) :    /* Day */
                          ((planet_info[0].alt > -6) ? Cscheme(cDkRed, cWhite, cBlack, cBlack) : /* Dawn/Sunset */
                              Cscheme(cDkBlue, cWhite, cBlack, cBlack)                       /* Twilight */
                          )
                        );
        } else {
            skycolour = Cscheme(cBlack, cWhite, cBlack, cBlack);
        }

        initbmp(bmi.bmiHeader.biWidth, bmi.bmiHeader.biHeight,
               tmsize, tmsize, skycolour);

        initmoon(MoonIcon);
        updMoonIcon(jt);

#define WindowBorder 0
        setWindowOffset(WindowBorder, WindowBorder);
        paintSky(jt, tmsize - (WindowBorder * 2));
        setWindowOffset(0, 0);

        /*  If this is a horizon view, draw terrain and scenery if
            requested, and label the cardinal points at
            the bottom of the window.  */

        if (skyMode == paintHOR) {
            int i;

            if (horShowTerrain) {
                drawTerrain(tmsize, jt, horazi, horSceneryRoughness,
                            horShowScenery);
            }

            setColour(CschemeD(cYellow));
            setFont("roman", 10, 0, ALIGN_CENTRE | ALIGN_BOTTOM);
            for (i = 0; i < 16; i++) {
                int x, y, in;

                xform(5.0, 360 - (i * 22.5), &x, &y, &in);
                if (in) {
                    drawText(cardinals[i], x, tmsize - 5);
                }
            }
        }

#ifdef PPM_SUPPORT
        if (wppm) {
            dump_ppm("-", tmbmap, tmbits);
        } else
#endif
        {
            int tmrowlen = (tmbmap->bmiHeader.biWidth + 3) & ~3;

            gifout(tmbmap->bmiHeader.biWidth, tmbmap->bmiHeader.biHeight, tmrowlen,
                   tmbmap->bmiColors, tmbmap->bmiHeader.biClrUsed,
                   cBorder,
                   tmbits, ofile);
        }

   }

    /*  Create HTML reply file on standard output.  */

    if (html) {
        static char *weekdays[] = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
        int mademap = FALSE;
        char *frab = NULL;

        sprintf(tbuf, "%s %d %s %d %d:%02d", weekdays[ctim.tm_wday],
            ctim.tm_year + 1900, mname[ctim.tm_mon], ctim.tm_mday,
            ctim.tm_hour, ctim.tm_min);

        printf("\
<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>\n\
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n\
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n\
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n\
<head>\n\
<title>\n\
%s\n\
</title>\n\
</head>\n\
<body%s>\n\
",      viewdesc,
        skyColourScheme == SCS_NIGHT_VISION ?
            " bgcolor=\"#000000\" text=\"#FF0000\""
                                            : "");

        /*  Create client-size image map to allow click-to-pan in the image.  */

        initialiseSkyTransform(jt, tmsize - (WindowBorder * 2));
#define NCELLS  8

        {
            int x, y, frabl = 4096;
            char *elements = getenv("WWW_elements");

            /* Even the term "brutal hack" is a euphemism for the code which
               follows.  You see, when the user clicks in the image map or
               to zoom, we want to produce a map with the altered viewpoint
               but with all the other settings identical to that of the
               map containing the link.  Well, we *COULD* include code to
               output each of the options individually, but then every
               time we added a new control, we'd have to remember to add
               corresponding code here, or else the new control would be
               forgotten on a viewpoint link.

               Instead, we just blindly riffle through the environment
               and find all the "WWW_" variables created by uncgi.
               Dropping the ones known to change with the viewpoint
               leaves us with the complete set of parameters passed
               to this invocation of the program, so we can then
               re-encode them as a URI which is hammered into the links.  */

            frab = (char *) malloc(frabl);
            if (frab != NULL) {
                int e;
                char *v;

                *frab = 0;
                for (e = 0; (v = environ[e]) != NULL; e++) {
                    char *es;

                    if ((strncmp(v, "WWW_", 4) == 0) && ((es = strchr(v, '=')) != NULL)) {
                        if (strncmp(v + 4, "lat=", 4) != 0 &&
                            strncmp(v + 4, "ns=", 3) != 0 &&
                            strncmp(v + 4, "lon=", 4) != 0 &&
                            strncmp(v + 4, "ew=", 3) != 0 &&
                            strncmp(v + 4, "aim=", 4) != 0 &&
                            strncmp(v + 4, "azimuth=", 8) != 0 &&
/*                          strncmp(v + 4, "elements=", 9) != 0 && */
                            strncmp(v + 4, "fov=", 4) != 0) {
                            char *s = escape_url(es + 1);
                            char ses;

                            ses = es[1];
                            es[1] = 0;
                            if ((strlen(frab) + strlen(v + 4) + strlen(s) + 10) > frabl) {
                                frab = realloc(frab, frabl *= 2);
                                if (frab == NULL) {
                                    goto wretch;
                                }
                            }
                            if (s != NULL) {
                                sprintf(frab + strlen(frab), "&amp;%s%s", v + 4, s);
                                free(s);
                            }
                            es[1] = ses;
                        }
                    }
                }
                strcat(frab, "\"");
/* strcat(frab, "\" />\n"); */

                if (elements != NULL && strlen(elements) > 0) {
                    qelt = escape_url(elements);
                }

                /* Now actually generate the client-side image map. */

                numwins = 1;
                mapwin[0] = &telwin;
                printf("<map name=\"panmap\" id=\"panmap\">\n");
                for (x = 0; x < tmsize; x += tmsize / NCELLS) {
                    for (y = 0; y < tmsize; y += tmsize / NCELLS) {
                        double cellra, celldec;
                        int nx =  min(x + tmsize / NCELLS, tmsize - 1),
                            ny = min(y + tmsize / NCELLS, tmsize - 1);
                        if (invxform((x + nx) / 2, (y + ny) / 2, &celldec, &cellra) >= 0) {

                            /*  If we're generating a horizon view we need to
                                transform from alt-azimuth coordinates back
                                to the equatorial coordinates the telescope
                                requires.  */

                            if (skyMode == paintHOR) {
                                double alt, az, lha, ra, dec;

                                az = fixangle(180 - cellra);
                                alt = celldec;
                                lha = atan2(sin(dtr(az)), cos(dtr(az)) * sin(dtr(siteLat)) +
                                        tan(dtr(alt)) * cos(dtr(siteLat)));
                                dec = asin(sin(dtr(siteLat)) * sin(dtr(alt)) - cos(dtr(siteLat)) *
                                        cos(dtr(alt)) * cos(dtr(az)));
                                ra = fixangle(rtd(lham - lha));
                                dec = fixangle(rtd(dec));
                                celldec = dec;
                                cellra = ra;
                            }
                            printf("<area shape=\"rect\" coords=\"%d,%d,%d,%d\" alt=\"%.4f,%.3f\" href=\"/cgi-bin/Yourtel" TESTFLAG "?",
                                x, y, nx, ny, celldec, cellra);
                            printf("lat=%.4f&amp;ns=%s&amp;lon=%.3f&amp;fov=%.3f%s />\n", abs(celldec),
                                celldec < 0 ? "South" : "North", cellra, telfov, frab);
                        }
                    }
                }
                printf("</map>\n");
                mademap = TRUE;
            }
        }
wretch:;

        /*  Write the HTML to include the image in the reply document.  */

        printf("<center><h1>%s<br />%s UTC</h1></center>\n", viewdesch, tbuf);
        printf("<center>\n");
        printf("<a href=\"/yoursky/help/sky.html\"><em><font size=\"-1\">Explain symbols in the map.</font></em></a><br />\n");

        if (stateless) {
            printf("<img src=\"/cgi-bin/Your%s" TESTFLAG "?di=%s\" %swidth=\"%d\" height=\"%d\" border=\"0\" alt=\"View of RA: %s Dec: %s at %s UTC\" /><br />\n",
                skyMode == paintHOR ? "horizon" : "tel",
                packargs(argc, argv),
                mademap ? "usemap=\"#panmap\" ismap=\"ismap\" " : "",
                tmsize, tmsize,
                editRA(vlon), eddec(vlat), tbuf);
        } else {

            if (mademap) {
                printf("<a href=\"/yoursky/nomap.html\">\n");
            }

#ifdef CACHE_WARNING
            write_cache_image_warning(stdout);
#endif
            printf("<img src=\"%s\" %swidth=\"%d\" height=\"%d\" border=\"0\" alt=\"View of RA: %s Dec: %s at %s UTC\" /><br />\n",
                cacheName, mademap ? "usemap=\"#panmap\" ismap=\"ismap\" " : "", tmsize, tmsize,
                editRA(vlon), eddec(vlat), tbuf);
            if (mademap) {
                printf("</a>\n");
            }
        }
        printf("<table width=\"%d\">\n", tmsize);
        printf("<tr><td align=\"left\" width=\"20%%\">");
        if (skyMode == paintHOR) {
            printf("<a href=\"/cgi-bin/Yourhorizon" TESTFLAG "?lat=%.4f&amp;ns=%s&amp;lon=%.3f&amp;ew=%s&amp;fov=%.3f&amp;azimuth=%.3f%s><b>Pan left</b></a></td>",
                abs(siteLat), siteLat < 0 ? "South" : "North",
                abs(siteLon), siteLon < 0 ? "East" : "West", telfov,
                fixangle(horazi - telfov),
                frab);
            printf(" <td align=\"center\" width=\"60%%\"><em>Click in map to aim telescope.</em></td>\n");
            printf(" <td align=\"right\" width=\"20%%\">");
            printf("<a href=\"/cgi-bin/Yourhorizon" TESTFLAG "?lat=%.4f&amp;ns=%s&amp;lon=%.3f&amp;ew=%s&amp;fov=%.3f&amp;azimuth=%.3f%s><b>Pan right</b></a></td>",
                abs(siteLat), siteLat < 0 ? "South" : "North",
                abs(siteLon), siteLon < 0 ? "East" : "West", telfov,
                fixangle(horazi + telfov), frab);
        } else {
#define ZoomFactor  sqrt(2.0)         /* Zoom in/out factor */
            printf("<a href=\"/cgi-bin/Yourtel" TESTFLAG "?lat=%.4f&amp;ns=%s&amp;lon=%.3f&amp;fov=%.3f%s><b>Zoom in</b></a></td>",
                abs(teldec), teldec < 0 ? "South" : "North", telra, telfov / ZoomFactor, frab);
            printf(" <td align=\"center\" width=\"60%%\"><em>Click in map to pan.</em></td>\n");
            printf(" <td align=\"right\" width=\"20%%\">");
#define MAXfov 60
            if (telfov < MAXfov) {
                printf("<a href=\"/cgi-bin/Yourtel" TESTFLAG "?lat=%.4f&amp;ns=%s&amp;lon=%.3f&amp;fov=%.3f%s><b>Zoom out</b></a>",
                    abs(teldec), teldec < 0 ? "South" : "North", telra, min(MAXfov, telfov * ZoomFactor), frab);
            }
            printf("</td>");
        }
        printf("</tr>\n</table>\n");
        if (skyMode == paintHOR) {
            printf("<br /><a href=\"/cgi-bin/Yoursky" TESTFLAG "?lat=%.4f&amp;ns=%s&amp;lon=%.3f&amp;ew=%s&amp;fov=45&amp;azimuth=0%s><b>View sky map for this observing site.</b></a>\n",
                abs(siteLat), siteLat < 0 ? "South" : "North",
                abs(siteLon), siteLon < 0 ? "East" : "West", frab);
        }
        printf("</center>\n");

#ifdef ShowCPUtime
        {
            struct rusage u;

            getrusage(RUSAGE_SELF, &u);
            printf("CPU time: %d seconds.<br />\n", u.ru_utime.tv_sec + u.ru_stime.tv_sec);
        }
#endif
        writepost(skyMode == paintHOR ? vlat : teldec,
                  skyMode == paintHOR ? vlon : telra, tmsize, jt, qelt);
        if (qelt != NULL) {
            free(qelt);
        }
    }

    if (directCall) {
        FILE *postamble = fopen(locfile(skyMode == paintHOR ? "yourizon-post.html" : "yourtel-post.html"), "r");

#ifdef SHOW_ARGUMENTS
{
int i;

printf("<pre>\nEnvironment Variables:\n\n");
for (i = 0; environ[i] != NULL; i++) {
    printf("    %s\n", environ[i]);
}
printf("\nCommand Line Arguments:\n\n");
for (i = 1; i < argc; i++) {
    printf("    %d: %s\n", i, argv[i]);
}
printf("</pre>\n");
}
#endif

        if (postamble != NULL) {
            char s[1024];
            int l;

            while ((l = fread(s, 1, sizeof s, postamble)) > 0) {
                fwrite(s, 1, l, stdout);
            }
            fclose(postamble);
        }
    }

    if (ofile != stdout) {
        fclose(ofile);
    }
#ifdef DBLOG
    fclose(db);
#endif
    return 0;
}
