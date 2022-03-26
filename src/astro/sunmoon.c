/*

                               Your Sky

                  Build Moon icon for current phase

*/

#include "vplanet.h"

static unsigned char template[32][32];/* Moon icon template */
static int mIcon;                     /* Moon icon number */

/*  INITMOON  --  Initialise moon icon.  */

void initmoon(int iconNo)
{
    int t = iconNo * 32, x, y;

    mIcon = iconNo;
    for (x = 0; x < 32; x++) {
        for (y = 0; y < 32; y++) {
            template[x][y] = *PixA(x + t, y);
        }
    }
}

/*  ICONMOON  --  Construct icon for moon, given phase of moon.  */

static void iconmoon(int width, int CENTER,
                     int IRADIUS, int OFFSET,
                     double ph, int mm, int dd)
{
    int i, j, lx, rx;
    double cp, xscale, RADIUS = IRADIUS;
    int t = mIcon * 32, x, y;

    /*  Restore template icon from copy saved by initmoon().  */

    for (x = 0; x < 32; x++) {
        for (y = 0; y < 32; y++) {
            *PixA(x + t, y) = template[x][y];
        }
    }

    /*  Adjust the template icon to show the current phase.  */

    xscale = cos(2 * PI * ph);
    for (i = 0; i < IRADIUS; i++) {
        cp = RADIUS * cos(asin((double) i / RADIUS));
        if (ph < 0.5) {
            rx = (int) (CENTER + cp);
            lx = (int) (CENTER + xscale * cp);
        } else {
            lx = (int) (CENTER - cp);
            rx = (int) (CENTER - xscale * cp);
        }
/*fprintf(stderr, "Ph = %.2f, i = %d, rx = %d, lx = %d\n", ph, i, rx, lx); */

        /* We now know the left and right endpoints of the scan line
           for this y coordinate.  Clear pixels corresponding to the
           dark portion of the Moon at the given phase.  We have to be
           careful to clear pixels inside the Moon to black while
           leaving those outside transparent. */

#define InMoon(x) (((x) >= (CENTER - cp)) && ((x) <= (CENTER + cp))))
#define Mpix(x) ((InMoon(x) ? cBlack : transparent)

        if (lx > 0) {
            for (j = 0; j < lx; j++) {
                *PixA(t + j, OFFSET + i) = Mpix(j);
            }
            if ((rx + 1) < width) {
                for (j = 0; j < width - rx; j++) {
                    *PixA(t + rx + j + 1, OFFSET + i) = Mpix(rx + 1 + j);
                }
                if (i != 0) {
                    for (j = 0; j < lx; j++) {
                        *PixA(t + j, OFFSET - i) = Mpix(j);
                    }
                    if ((rx + 1) < width) {
                        for (j = 0; j < width - rx; j++) {
                            *PixA(t + rx + j + 1, OFFSET - i) = Mpix(rx + 1 + j);
                        }
                    }
                }
            }
        }
    }

    /*  If it's July 20th display the Apollo 11 Commemorative Red Dot
        at Tranquility Base.  Otherwise, just show the regular mare
        floor.  */

    if ((skyColourScheme == SCS_COLOUR) && (mm == 7) && (dd == 20)) {
        *PixA(t + 20, 14) = cRed;
    }
}

/*  UPDMOONICON  --  Update Moon icon if required.  */

void updMoonIcon(double jd)
{
    double p, aom, cphase, cdist, cangdia, csund, csuang;
    long yy;
    int mm, dd;

    jyear(jd, &yy, &mm, &dd);
    p = phase(jd, &cphase, &aom, &cdist, &cangdia, &csund, &csuang);
    iconmoon(32, /* X ctr */ 16, /* X radius */ 14,
                                 /* Y centre */ 14, p, mm, dd);
}
