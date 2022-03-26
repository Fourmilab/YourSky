/*

           Planet information panel

*/

#include "vplanet.h"

/*  CALCPLANET  --  Calculate planetary positions and altitude and azimuth from
                    viewer's position.  */

void calcPlanet(double jd)
{
    int i;
    double igmst, latsin, latcos;

    planets(jd, 0xFFFF);
    igmst = gmst(jd);
    latsin = sin(dtr(siteLat));
    latcos = cos(dtr(siteLat));
    for (i = 0; i <= (aTracked ? 10 : 9); i++) {
        planet_info[i].lha = dtr(fixangle((igmst * 15) - siteLon - planet_info[i].ra));
        planet_info[i].az = rtd(atan2(sin(planet_info[i].lha), cos(planet_info[i].lha) * latsin -
                                tan(dtr(planet_info[i].dec)) * latcos));
        planet_info[i].alt = rtd(asin(latsin * sin(dtr(planet_info[i].dec)) +
                                latcos * cos(dtr(planet_info[i].dec)) * cos(planet_info[i].lha)));
    }
}

/*  REFRESHLASTALT  --  Refresh last altitude to determine rising from setting.  */

static double lastalt[11] = {-9999};        /* Last calculated altitudes */
static double lastsaltime = -9999;          /* Time of last altitude calculation */

static void refreshLastSalt(double jd)
{
    int i;

    calcPlanet(lastsaltime = (jd - (1.0 / 48.0)));
    for (i = 0; i <= (aTracked ? 10 : 9); i++) {
        lastalt[i] = planet_info[i].alt;
    }
}

/*  ESIGN  --  Edit sign for a double in XHTML.  If the number is
               negative an XHTML "&minus;" entity is returned,
               otherwise a null string.  */

static char *esign(const double v)
{
    return (v < 0) ? "&minus;" : "";
}

/*  UPDATEPLANET  --  Recalculate planet positions (if necessary) and update
                      the fields in the dialogue.  */

void updatePlanet(double jd, FILE *ofile, char *obSite, char *qelt)
{
    int i;
    double d, md, mangdia, suangdia, setalt, upalt;
    static char *plnames[] = { "Sun", "Mercury", "Venus", "Moon",
                               "Mars", "Jupiter", "Saturn", "Uranus",
                               "Neptune", "Pluto", "?" };

    if (jd < VSOP87_INVALID) {
        if (aTracked) {
            plnames[10] = escape_html_content(ast_info.Name);
        }
        fprintf(ofile, "<center>\n<table border=\"border\" cellpadding=\"3\">\n");
        fprintf(ofile, "<tr><td>&nbsp;</td><th>Right<br />Ascension</th><th>Declination</th><th>Distance<br />(<span title=\"Astronomical units (149,597,871 km)\">AU</span>)</th></tr>\n");

        calcPlanet(jd);
        phase(jd, &d, &d, &md, &mangdia, &d, &suangdia);

        for (i = 0; i <= (aTracked ? 10 : 9); i++) {
            double m, n;
            char dist[128];

            /* If outside the known calculable orbit of Pluto, hide the bogus numbers */
            if (i == 9 && planet_info[i].hrv <= 0) {
                continue;
            }

            m = fixangle(planet_info[i].ra) / 15;
            n = abs(planet_info[i].dec);
            if (i == 3) {
                sprintf(dist, "%.1f <span title=\"Earth radii (6378.14 km)\">ER</span>", md / EarthRad);
            } else {
                sprintf(dist, "%.3f", planet_info[i].dist);
            }
            setalt = -0.5667;
            upalt = 0;

            fprintf(ofile, "<tr><th><a href=\"/cgi-bin/uncgi/Yourtel" TESTFLAG "?aim=%d&amp;z=1", i);
            if (qelt != NULL) {
                printf("&amp;elements=%s", qelt);
            }
            fprintf(ofile, "\">%s</a></th>", plnames[i]);
            fprintf(ofile, "<td align=\"right\">%dh %dm %ds</td><td align=\"right\">%s%d\260 %02.1f'</td>\
<td align=\"right\">%s</td></tr>\n",
                (int) m, ((int) (m * 60)) % 60, (int) fmod(m * 3600, 60.0),    /* RA */
                planet_info[i].dec < 0 ? "&minus;" : "+", (int) n, fmod(n * 60, 60), /* Dec */
                dist);
        }
        fprintf(ofile, "</table>\n</center>\n<p />\n");
    }
}

/*  UPDATEPLANETCOMPLETE  --  Recalculate planet positions and output
                              the ephemeris.  This "complete" version
                              is used when the observing site is known.  */

void updatePlanetComplete(double jd, FILE *ofile, char *obSite, char *qelt)
{
    int i;
    double d, md, mangdia, suangdia, setalt, upalt;
    static char *plnames[] = { "Sun", "Mercury", "Venus", "Moon",
                               "Mars", "Jupiter", "Saturn", "Uranus",
                               "Neptune", "Pluto", "?" };

#define Set(x) memcpy(obuf + (x) + (8 - strlen(tbuf)), tbuf, strlen(tbuf))

    if (jd < VSOP87_INVALID) {
        if (aTracked) {
            plnames[10] = escape_html_content(ast_info.Name);
        }
        fprintf(ofile, "<center>\n<table border=\"border\" cellpadding=\"3\">\n");
        fprintf(ofile, "<tr><th rowspan=\"2\">&nbsp;</th><th rowspan=\"2\">Right<br />Ascension</th><th rowspan=\"2\">Declination</th><th rowspan=\"2\">Distance<br />(<span title=\"Astronomical units (149,597,871 km)\">AU</span>)</th><th colspan=\"2\">From %s:</th></tr>\n", obSite);
        fprintf(ofile, "<tr><th>Altitude</th><th>Azimuth</th></tr>\n");

        if (lastsaltime > jd || ((jd - lastsaltime) > (1.0 / 24.0))) {
            refreshLastSalt(jd);
        }
        calcPlanet(jd);
        phase(jd, &d, &d, &md, &mangdia, &d, &suangdia);
        lastsaltime = jd;

        for (i = 0; i <= (aTracked ? 10 : 9); i++) {
            double m, n;
            char dist[128], upset[20];

            /* If outside the known calculable orbit of Pluto, hide the bogus numbers */
            if (i == 9 && planet_info[i].hrv <= 0) {
                continue;
            }

            m = fixangle(planet_info[i].ra) / 15;
            n = abs(planet_info[i].dec);
            if (i == 3) {
                sprintf(dist, "%.1f <span title=\"Earth radii (6378.14 km)\">ER</span>", md / EarthRad);
            } else {
                sprintf(dist, "%.3f", planet_info[i].dist);
            }
            setalt = -0.5667;
            upalt = 0;

            /* (Approximately) compensate for the effect of refraction by
               the Earth's atmosphere so that "rising" and "setting" appears
               at the time the object actually becomes visible or disappears
               rather than considering pure geometry of touching the
               horizon.  These calculations follow the guidance of Meeus in
               chapter 14 of Astronomical Algorithms.

               Note that the Sunrise is considered to be the first appearance
               of the limb above the horizon.

               Moonrise is even more complicated.  First of all, in addition to
               refraction we must also compensate for the parallax of the
               Moon which, in turn, varies due to the eccentricity of the Moon's
               orbit.  Further (departing from normal astronomical convention),
               if we wish to also time moonrise from first appearance of the
               limb rather than the centre of the disc (we adopt this nonstandard
               criterion to prevent squawks from folks who see the Moon rising
               out their window before we've labeled it as "rising"), we must add
               the semidiameter of the Moon to our refraction correction.  That,
               of course, *also* changes from perigee to apogee, so there goes
               another correction into the stew.  Ain't this fun?

               Then there's the question of how long "rising" and "setting"
               lasts.  For a planet it's almost instantaneous, but we
               compromise and consider rising and setting to be the interval
               between refraction-corrected rise/set and geometric (about half
               a degree to the horizon.  For the Sun and Moon, we must adjust the
               end of rise/set times for the diameter of the disc.  */

            if (i == 0) {               /* Sun */
                setalt -= suangdia / 2;
                upalt = setalt + suangdia;
            } else if (i == 3) {        /* Moon */
#define msmax       384401.0                    /* Semi-major axis of Moon's orbit in km */
#define mparallax   0.9507                      /* Parallax at distance msmax from Earth */
                setalt += 0.7275 * (mparallax * (md / msmax));
                setalt -= mangdia / 2;
                upalt = setalt + mangdia;
            }
            if ((planet_info[i].alt > setalt) && (planet_info[i].alt < upalt)) {
                strcpy(upset, (planet_info[i].alt > lastalt[i]) ? "Rising" : "Setting");
            } else {
                if (planet_info[i].alt > upalt) {
                    strcpy(upset, (abs(planet_info[i].lha) < dtr(0.5)) ?
                             "Transit" : "Up");
                } else {
                    strcpy(upset, "Set");
                }
            }
            lastalt[i] = planet_info[i].alt;        /* Save last altitude */

            fprintf(ofile, "<tr><th><a href=\"/cgi-bin/uncgi/Yourtel" TESTFLAG "?aim=%d&amp;z=1", i);
            if (qelt != NULL) {
                printf("&amp;elements=%s", qelt);
            }
            fprintf(ofile, "\">%s</a></th>", plnames[i]);
            fprintf(ofile, "<td align=\"right\">%dh %dm %ds</td><td align=\"right\">%s%d\260 %02.1f'</td>\
<td align=\"right\">%s</td><td align=\"right\">%s%.3f</td><td align=\"right\">%s%.3f</td><td>%s</td></tr>\n",
                (int) m, ((int) (m * 60)) % 60, (int) fmod(m * 3600, 60.0),    /* RA */
                planet_info[i].dec < 0 ? "&minus;" : "+", (int) n, fmod(n * 60, 60), /* Dec */
                dist,                                                          /* Distance */
                esign(planet_info[i].alt), abs(planet_info[i].alt),            /* Altitude */
                esign(planet_info[i].az), abs(planet_info[i].az),              /* Azimuth */
                upset);                                                        /* Status */
        }
        fprintf(ofile, "</table>\n</center>\n<blockquote>\n");
        fprintf(ofile, "<small>Azimuth in the above table\n");
        fprintf(ofile, "follows the astronomical convention: zero\n");
        fprintf(ofile, "degrees is South with positive angles toward the\n");
        fprintf(ofile, "West and negative angles toward the East.</small>\n");
        fprintf(ofile, "</blockquote><p />\n");
    }
}
