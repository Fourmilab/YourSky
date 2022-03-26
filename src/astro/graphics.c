/*

                               Your Sky

                           Graphics Output

*/

#include "vplanet.h"

static int colourIndex = 0, xbias = 0, ybias = 0;

/*  Set window offset.  */

void setWindowOffset(int x, int y)
{
    xbias = x;
    ybias = y;
}

/*  Set current drawing colour.  */

int setColour(int newColour)
{
    int oc = colourIndex;

    colourIndex = newColour;
    return oc;
}

/*  Vector drawing routines.  */

void drawVector(int x1, int y1, int x2, int y2)
{
/*  printf("Graphics: draw vector from %d,%d to %d,%d\n", x1, y1, x2, y2); */
    psrvector(xbias + x1, ybias + y1, xbias + x2, ybias + y2, colourIndex);
}

/*  Text drawing routines.  */

static int t_size, t_angle, t_greek, t_align;

void setFont(char *fontname, int size, int angle, int align)
{
/*  printf("Graphics: selected font %s, size = %d, angle = %d\n", fontname, size, angle); */
    t_size = size;
    t_angle = angle;
    t_align = align;
    t_greek = strcmp(fontname, "greek") == 0;
}

/*  Note that since drawText ultimately calls drawVector, the
    xbias and ybias should not be applied here--drawVector will
    eventually apply them.  */

void drawText(char *text, int x, int y)
{
/*  printf("Graphics: draw text \"%s\" at %d,%d\n", text, x, y); */

    /* If the Greek font is selected, transform letters into
       the region of our unified font where the Greek
       characters live. */

    if (t_greek) {
        char *cp = text;
        char c;

        while ((c = *cp) != 0) {
            if (c >= 'A' && c <= 'Z') {
                *cp = 128 + (c - 'A');
            }
            if (c >= 'a' && c <= 'z') {
                *cp = 160 + (c - 'a');
            }
            cp++;
        }
    }

#ifdef NEEDED
    /* Transform ISO degree sign into our nonstandard mapping. */

    {
        char *cp = text;
        char c;

        while ((c = *cp) != NULL) {
            if (c == '°') {
                *cp = 127;
            }
            cp++;
        }
    }
#endif

    /*  If non-default (left justified, baseline) alignment is
        requested, obtain the extents of the text and offset
        the start point to achieve the requested alignment,
        given the angle at which the text is to be drawn.  */

    if (t_align != 0) {
        int left, top, right, bottom, dx = 0, dy = 0;

        psr_text_box(text, t_size, 0, &left, &top, &right, &bottom);
        if (t_align & ALIGN_CENTRE) {
            dx -= (right - left) / 2;
        } else if (t_align & ALIGN_RIGHT) {
            dx -= (right - left);
        }

        if (t_align & ALIGN_TOP) {
            dy -= top;
        } else if (t_align & ALIGN_BOTTOM) {
            dy += bottom;
        } else if (t_align & ALIGN_MIDDLE) {
            dy += (top - bottom) / 2;
        }

/*fprintf(stderr, "Drawing %s, l=%d, r=%d, t=%d, b=%d, dx = %d, dy = %d\n", text, left, right, top, bottom, dx, dy); */
        x += dx * cos(dtr((double) t_angle)) + dy * sin(dtr((double) t_angle));
        y += dy * cos(dtr((double) t_angle)) - dx * sin(dtr((double) t_angle));
    }

    psr_text(text, x, y, t_size, t_angle);
}

/*  Icon drawing routines.  */

/* ARGSUSED */
void drawIcon(char *iconFile, int iconNo, int x, int y)
{
/*  printf("Graphics: draw icon %d from \"%s\" at %d,%d\n", iconNo, iconFile, x, y); */
    psricon(iconNo, xbias + x, ybias + y);
}
