#include "vplanet.h"

#include "project.h"

#define PI 3.14159265358979323846

#define fixangle(a) ((a) - 360.0 * (floor((a) / 360.0)))  /* Fix angle    */
#define fixangr(a)  ((a) - (PI*2) * (floor((a) / (PI*2))))  /* Fix angle in radians*/
#define dtr(x) ((x) * (PI / 180.0))                       /* Degree->Radian */
#define rtd(x) ((x) / (PI / 180.0))                       /* Radian->Degree */

/* PI / 180 = .0174532925199 */
#define DCOS(x) (cos((x) * .0174532925199))
#define DSIN(x) (sin((x) * .0174532925199))
#define DTAN(x) (tan((x) * .0174532925199))
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#define MIN(a,b) ((a) < (b) ? (a) : (b))

mapwindow skywin, telwin, horwin;

mapwindow *mapwin[3] = { &skywin, &telwin, &horwin };
int numwins = 3;


/* exportable */
static double xf_west, xf_east, xf_north, xf_south, xf_bottom;
static int xf_xcen, xf_ycen, xf_ybot;
static int xf_w_left, xf_w_right, xf_w_top, xf_w_bot;

double xf_c_scale;

/* local storage */
static int xfs_proj_mode;
/* sin_dlcen = sin (phi0) = sin (declination of center), cos_dlcen similar */
static double xfs_ra_cen, xfs_dl_cen, sin_dlcen, cos_dlcen, chart_scale;
static double xfs_scale;                /* Formerly yscale */
static double xfs_vinv, xfs_hinv;
static int xfs_wide_warn;

/*  Forward functions.  */

static void init_gt(mapwindow *win);
static void do_gt(double lat, double lon, double *xloc, double *yloc, double *r_theta);
static void inv_gt(double x, double y, double *latp, double *lonp);

/* TRANSFORMATION FUNCTIONS */

/**
    stereographic projection:
        Imagine object projected on a sphere of radius 1.
        Center of chart is against a piece of paper.
        You are looking at the center of the chart through the
                center of the sphere.
        The objects are seen projected on the piece of paper.

    to do it:
        1) get angle from center to object, call this theta.
        2) get direction from center to object, call this phi.
        3) projection of object will be 2*tan(theta/2) from the center
                of the paper, in the direction phi.

    to do steps 1 & 2, use alt-azimuth formula, with theta = 90 - alt.

    scale: when theta = scale, projected object = min(ww,wh)/2 from center.
        i.e. 2*tan(scale/2)*xfs_scale = min(ww,wh)/2.
        or xfs_scale = (min(ww,wh)/2)/(2*tan(scale/2))
                  = min(ww,wh)/(4*tan(scale/2))
        put factor of 2 in xfs_scale to save a little computation
           xfs_scale = min(ww,wh)/(2*tan(scale/2))
        R = tan(theta/2)*xfs_scale.
*/
/**
    alt-azimuth:
      sin(alt) = sin(obs_lat)sin(obj_dl) + cos(obs_lat)cos(obj_dl)cos(hour)

      cos(azi) = (cos(obs_lat)sin(obj_dl) - sin(obs_lat)cos(obj_dl)cos(hour))
                        / cos(alt)
      sin(azi) = (-cos(obj_dl)sin(hour)) / cos(alt)
      tan(azi) = -cos((obj_dl)sin(hour)
              / (cos(obs_lat)sin(obj_dl) - sin(obs_lat)cos(obj_dl)cos(hour))


        with alt = 90 - theta, azi = phi, hour = lon - racen, obj_dl =lat,
                and obs_lat = dlcen, this becomes:

        cos(theta) = sin(dlcen)sin(lat) + cos(dlcen)cos(lat)cos(lon - racen)

        tan(phi) = -cos(lat)sin(lon - racen)
              / (cos(dlcen)sin(lat) - sin(dlcen)cos(lat)cos(lon - racen))


        racen and dlcen are constants for a chart, so introduce variables
            racen, sin_dlcen, cos_dlcen.
            also add chart_scale which is chart->scale in radians.
        initxform sets these static variables.



    Other projections from book.
**/


int initxform(mapwindow *win)
{
  double tscale = 0;

  xfs_proj_mode = win->proj_mode;

  if (win->scale <= 0.0)
    return FALSE;
  if (win->height == 0)
    return FALSE;

  xfs_ra_cen = win->racen;
  xfs_dl_cen = win->dlcen;
  xf_xcen = win->x_offset + (win->width) / 2;
  xf_ycen = win->y_offset + (win->height) / 2;
  xf_ybot = win->y_offset;

  xf_north = (win->dlcen + win->scale / 2);
  xf_south = (win->dlcen - win->scale / 2);

  if (xf_north > 90.0)
    xf_north = 90.0;
  if (xf_south < -90.0)
    xf_south = -90.0;

  if (win->invert) {
    xfs_vinv = -1.0;
/*    xfs_hinv = 1;         */
    xf_bottom = xf_north;
  } else {
    xfs_vinv = 1.0;
/*    xfs_hinv = -1;        */
    xf_bottom = xf_south;
  }

  if (win->mirror) {
    xfs_hinv = -1;
  } else {
    xfs_hinv = 1;
  }

  xf_east  = win->racen + tscale;
  xf_west  = win->racen - tscale;

  /* set warning, may have problems in SANSONS or RECTANGULAR
     with lines which should wrap around */
    xfs_wide_warn = FALSE;


  xf_w_left = win->x_offset;
  xf_w_right = win->x_offset + win->width;
  xf_w_bot = win->y_offset;
  xf_w_top = win->y_offset + win->height;

  switch (xfs_proj_mode) {
      case GNOMONIC:
      case ORTHOGR:
      case STEREOGR:
        sin_dlcen = DSIN(win->dlcen);
        cos_dlcen = DCOS(win->dlcen);
        chart_scale = win->scale * .0174532925199; /* Radians */
        break;

  }

/* xf_c_scale is the size in degrees which one pixel occupies on the map */
/* xfs_scale is the conversion factor for size of the picture
     (= R in some formulas for stereographic,
     gnomonic and orthographic projections) */
  if (xfs_proj_mode == STEREOGR) {
    xfs_scale = MIN(win->height, win->width) / (4.0 * DTAN(win->scale / 2.0));
    xf_c_scale = win->c_scale = 1.0 / (2.0 * DTAN(0.5) * xfs_scale);
  } else if (xfs_proj_mode == GNOMONIC) {
    xfs_scale = MIN(win->height, win->width) / (2.0 * DTAN(win->scale / 2.0));
    xf_c_scale = win->c_scale = 1.0/(DTAN(1.0) * xfs_scale);
  }

  /* initialize gnomonic transform function */
  init_gt(win);

  return TRUE;
}



void xform(double lat, double lon, int *xloc, int *yloc, int *inregion)
{
  double theta, actheta, rac_l;
  double denom;
  double Dcoslat, Dsinlat, Dcosrac_l, Dsinrac_l;
  /* Dcoslat, Dsinlat: of object latitude in degrees = phi
     Dcosrac_l, Dsinrac_l: of object ra - longditude of center = d(lambda) */
  double xlocd, ylocd;  /* double precision for xloc and yloc */


  switch (xfs_proj_mode) {

  case STEREOGR:
    /* Stereographic projection */
    rac_l = lon - xfs_ra_cen;
    Dsinlat = DSIN(lat);
    Dcoslat = DCOS(lat);
    Dcosrac_l = DCOS(rac_l);
    Dsinrac_l = DSIN(rac_l);
    actheta = sin_dlcen * Dsinlat + cos_dlcen * Dcoslat * Dcosrac_l;
    if (actheta > 1.0)
        theta = 0.0;
    else if (actheta < -1.0)
        theta = 3.14159265358979323846;
    else theta = acos(actheta);

    *inregion = (theta <= chart_scale);
    if (*inregion) {
      denom = (1 + sin_dlcen * Dsinlat + cos_dlcen * Dcoslat * Dcosrac_l) / xfs_scale;
      *xloc = (int) (xf_xcen - 2 * xfs_hinv * Dcoslat * Dsinrac_l / denom + 0.5);
      *yloc = (int) (xf_ycen + 2 * xfs_vinv * (cos_dlcen * Dsinlat
                                 - sin_dlcen * Dcoslat * Dcosrac_l) / denom + 0.5);
    }
    break;


  case GNOMONIC:
    /* Gnomonic projection */
    rac_l = lon - xfs_ra_cen;
    Dsinlat = DSIN(lat);
    Dcoslat = DCOS(lat);
    Dcosrac_l = DCOS(rac_l);
    Dsinrac_l = DSIN(rac_l);

    actheta = sin_dlcen * Dsinlat + cos_dlcen * Dcoslat * Dcosrac_l;
    if (actheta > 1.0)
        theta = 0.0;
    else if (actheta < -1.0)
        theta = 3.14159265358979323846;
    else
        theta = acos(actheta);

    if (theta <= 1.57) { /* avoid wrapping */
      denom = (sin_dlcen * Dsinlat + cos_dlcen * Dcoslat * Dcosrac_l) / xfs_scale;
      *yloc = (int) (ylocd = xf_ycen +
        (int) xfs_vinv *
         (cos_dlcen * Dsinlat - sin_dlcen * Dcoslat * Dcosrac_l) / denom + 0.5);
      *xloc = (int) (xlocd = xf_xcen - xfs_hinv * Dcoslat * Dsinrac_l / denom + 0.5);
      *inregion = ((xlocd >= xf_w_left) && (xlocd <= xf_w_right)
                   && (ylocd <= xf_w_top) && (ylocd >= xf_w_bot));
    } else
        *inregion = FALSE;
    break;

  default:
    break;
  }
}

/* Given x and y of a point on the display,
   return the latitude and longitude */
int invxform(int x, int y, double *latp, double *lonp)
{
    int i;
    int winnum;
    mapwindow *win;

    /* temporaries to hold values set by initxform */
    double t_xf_west, t_xf_east, t_xf_north, t_xf_south, t_xf_bottom;
    int t_xf_w_left, t_xf_w_right, t_xf_w_top, t_xf_w_bot;
    int t_xf_xcen, t_xf_ycen, t_xf_ybot;
    double t_xf_c_scale;

    int t_xfs_proj_mode;
    double t_xfs_ra_cen, t_sin_dlcen, t_cos_dlcen, t_chart_scale;
    double t_xfs_scale;
    double t_xfs_vinv, t_xfs_hinv;


    double rho = 0;
    double R, theta;
    double l, m, n;
    double l_, m_, n_;

    *latp = 0.0;
    *lonp = 0.0;

    /* First, find which mapwindow the point is in */
    for (i = 0; i < numwins; i++) {
        if ((x >= mapwin[i]->x_offset) && (y >= mapwin[i]->y_offset)
            && (x <= (mapwin[i]->x_offset+mapwin[i]->width))
            && (y <= (mapwin[i]->y_offset+mapwin[i]->height)))
          /* point is in window i */
          break;
    }
    if (i == numwins)
        return -1; /* outside all windows */

    winnum = i;
    win = mapwin[winnum];

    /* Now, initialize inverse transformation for window winnum */
    t_xf_west = xf_west;
    t_xf_east = xf_east;
    t_xf_north = xf_north;
    t_xf_south = xf_south;
    t_xf_bottom = xf_bottom;

    t_xf_xcen = xf_xcen;
    t_xf_ycen = xf_ycen;
    t_xf_ybot = xf_ybot;

    t_xf_w_left = xf_w_left;
    t_xf_w_right = xf_w_right;
    t_xf_w_top = xf_w_top;
    t_xf_w_bot = xf_w_bot;

    t_xf_c_scale = xf_c_scale;

    t_xfs_proj_mode = xfs_proj_mode;

    t_xfs_ra_cen = xfs_ra_cen;

    t_sin_dlcen = sin_dlcen;
    t_cos_dlcen = cos_dlcen;
    t_chart_scale = chart_scale;

    t_xfs_scale = xfs_scale;

    t_xfs_vinv = xfs_vinv;
    t_xfs_hinv = xfs_hinv;

    initxform(win);

    /* Calculate lat and lon */
    switch (win->proj_mode) {

        case GNOMONIC:
        case ORTHOGR:
        case STEREOGR:
            x -= xf_xcen;
            y -= xf_ycen;
            y *= (int) xfs_vinv;
            x *= (int) xfs_hinv;

            R = sqrt((double) ((((long) x) * x) + (((long) y) * y)));
            theta = atan2((double) y, (double) x);

            /* rho is the angle from the center of the display to the object on the
               unit sphere. */
            switch (win->proj_mode) {
                case STEREOGR:
                  rho = 2.0 * atan(R / (2.0 * xfs_scale));
                  break;

                case GNOMONIC:
                  rho = atan(R / xfs_scale);
                  break;

            }

            /* transform from (rho, theta) to l m n direction cosines */
            l = sin(rho) * cos(theta);    /* rho and theta are in radians */
            m = sin(rho) * sin(theta);
            n = cos(rho);

            /* transform to new declination at center
               new axes rotated about x axis (l) */
            l_ = l;
            m_ = m * sin_dlcen - n * cos_dlcen;
            n_ = m * cos_dlcen + n * sin_dlcen;

            /* calculate lon and lat */
            *lonp = atan2(l_, m_) / 0.0174532925199 + xfs_ra_cen - 180.0;
            *latp = 90 - acos(n_) / 0.0174532925199;

            break;

        default:                      /* error */
            winnum = -1;
    }


    /* restore initxform's variables */
    xf_west = t_xf_west;
    xf_east = t_xf_east;
    xf_north = t_xf_north;
    xf_south = t_xf_south;
    xf_bottom = t_xf_bottom;

    xf_xcen = t_xf_xcen;
    xf_ycen = t_xf_ycen;
    xf_ybot = t_xf_ybot;

    xf_w_left = t_xf_w_left;
    xf_w_right = t_xf_w_right;
    xf_w_top = t_xf_w_top;
    xf_w_bot = t_xf_w_bot;

    xf_c_scale = t_xf_c_scale;

    xfs_proj_mode = t_xfs_proj_mode;

    xfs_ra_cen = t_xfs_ra_cen;

    sin_dlcen = t_sin_dlcen;
    cos_dlcen = t_cos_dlcen;
    chart_scale = t_chart_scale;

    xfs_scale = t_xfs_scale;

    xfs_vinv = t_xfs_vinv;
    xfs_hinv = t_xfs_hinv;

    if (*lonp >= 360.0)
        *lonp -= 360.0;
    if (*lonp < 0.0)
        *lonp += 360.0;

    return winnum;
}



/* Gnomonic transformation
   Used to draw vectors as great circles
   in Gnomonic transform a great circle is projected to a line.
*/
static double gt_sin_dlcen, gt_cos_dlcen, gt_chart_scale;
static double gt_scale;

/* endpoints of west and east boundaries in SANSONS and RECTANGULAR */
static double gt_wx1, gt_wy1, gt_wx2, gt_wy2;
static double gt_ex1, gt_ey1, gt_ex2, gt_ey2;

/* radius for STEREOGRAPHIC, GNOMONIC, and ORTHOGRAPHIC */
static double gt_r;

/* Can we clip to boundaries analytically? */
static int gt_use_boundaries;

static void init_gt(mapwindow *win)
{
    double adj;

    gt_use_boundaries = TRUE;

    gt_sin_dlcen = DSIN(win->dlcen);
    gt_cos_dlcen = DCOS(win->dlcen);
    gt_chart_scale = win->scale * .0174532925199; /* Radians */

    /* gt_scale is the conversion factor for size of the picture ( = R) */
    if (xfs_proj_mode == STEREOGR)
        gt_scale = MIN(win->height, win->width) / (2.0 * DTAN(win->scale));
    else
        gt_scale = MIN(win->height, win->width) / (2.0 * DTAN(win->scale / 2.0));

    adj = xf_c_scale * 0.9;         /* use boundaries slightly
                                       more restricted than full plot */

    /* calculate boundaries of region */
    switch (xfs_proj_mode) {

        case STEREOGR:
            gt_r = MIN(win->height, win->width) / 2.0 - 1;
            break;

        case GNOMONIC:
            gt_wx1 = gt_wx2 = xf_w_left - xf_xcen + 1;
            gt_ex1 = gt_ex2 = xf_w_right - xf_xcen - 1;
            gt_ey1 = gt_wy1 = xf_w_bot - xf_ycen + 1;
            gt_ey2 = gt_wy2 = xf_w_top - xf_ycen - 1;
            break;

        default:                      /* error */
            break;
    }
}


/* Note, returns xloc and yloc as doubles */
static void do_gt(double lat, double lon, double *xloc, double *yloc, double *r_theta)
{
    double theta, rac_l;
    double denom;
    double Dcoslat, Dsinlat, Dcosrac_l, Dsinrac_l;
    /* Dcoslat, Dsinlat: of object latitude in degrees = phi
     Dcosrac_l, Dsinrac_l: of object ra - longditude of center = d(lambda) */

    rac_l = lon - xfs_ra_cen;
    Dsinlat = DSIN(lat);
    Dcoslat = DCOS(lat);
    Dcosrac_l = DCOS(rac_l);
    Dsinrac_l = DSIN(rac_l);

    *r_theta =
        theta = acos(gt_sin_dlcen*Dsinlat + gt_cos_dlcen * Dcoslat * Dcosrac_l);

    if (theta <= 1.57) { /* avoid wrapping */
        denom = (gt_sin_dlcen * Dsinlat + gt_cos_dlcen * Dcoslat * Dcosrac_l) / gt_scale;
        *yloc = xfs_vinv *
            (gt_cos_dlcen * Dsinlat - gt_sin_dlcen * Dcoslat * Dcosrac_l) / denom;
        *xloc = xfs_hinv * (- Dcoslat * Dsinrac_l / denom);
    }
}

/* Given x and y of a point on the display,
   return the latitude and longitude */

static void inv_gt(double x, double y, double *latp, double *lonp)
{
    double rho;
    double R, theta;
    double l, m, n;
    double l_, m_, n_;

    y *= xfs_vinv;
    x *= xfs_hinv;

    *latp = 0.0;
    *lonp = 0.0;

    /* Calculate lat and lon */
    R = sqrt((double) ((((long) x) * x) + (((long) y) * y)));
    theta = atan2((double) y, (double) x);

    /* rho is the angle from the center of the display to the object on the
     unit sphere. */
    rho = atan(R / gt_scale);

    /* transform from (rho, theta) to l m n direction cosines */
    l = sin(rho) * cos(theta);      /* rho and theta are in radians */
    m = sin(rho) * sin(theta);
    n = cos(rho);

    /* transform to new declination at center
     new axes rotated about x axis (l) */
    l_ = l;
    m_ = m * gt_sin_dlcen - n * gt_cos_dlcen;
    n_ = m * gt_cos_dlcen + n * gt_sin_dlcen;

    /* calculate lon and lat */
    *lonp = atan2(l_, m_) / 0.0174532925199 + xfs_ra_cen - 180.0;
    if (n_ > 1) n_ = 1;
    if (n_ < -1) n_ = -1;
    *latp = 90 - acos(n_) / 0.0174532925199;

    if (*lonp >= 360.0)
        *lonp -= 360.0;
    if (*lonp < 0.0)
        *lonp += 360.0;
}

/*
 * clipping extentions (ccount)
 */

#define Fuz 0.1

static void quadrat(double a, double b, double c, double *x_1, double *x_2, int *n)
{
    double t;

    if (a == 0) {
        *n = 0;
    } else {
        t = b * b - 4 * a * c;
        if (t < 0) {
          *n = 0;
        } else if (t == 0) {
          *x_1 = -b/(2*a);
          *n = 1;
        } else {
          *x_1 = (-b + sqrt(t)) / (2 * a);
          *x_2 = (-b - sqrt(t)) / (2 * a);
          *n = 2;
        }
    }
}

static void gcmidpoint(double lat1, double lon1, double lat2, double lon2,
                       double *pmlat, double *pmlon)
{
    double l1, m1, n1;
    double l2, m2, n2;
    double l3, m3, n3;

    /* transform from (ra, dec) to l m n direction cosines */
    l1 = DCOS(lat1) * DCOS(lon1);
    m1 = DCOS(lat1) * DSIN(lon1);
    n1 = DSIN(lat1);

    l2 = DCOS(lat2) * DCOS(lon2);
    m2 = DCOS(lat2) * DSIN(lon2);
    n2 = DSIN(lat2);

    l3 = l1 + l2;
    m3 = m1 + m2;
    n3 = n1 + n2;
    n3 /= sqrt(l3 * l3 + m3 * m3 + n3 * n3);

    *pmlon = atan2(m3, l3) / 0.0174532925199;
    if ((*pmlon < 0) && (lon1 > 0) && (lon2 > 0))
        *pmlon += 360.0;
    *pmlat = asin(n3) / 0.0174532925199;
}

/* calculate and return the intersection point of two lines given
   two points on each line */
static void line_intersect(double x_1, double y_1, double x_2, double y_2,
                           double x_3, double y_3, double x_4, double y_4,
                           double *x, double *y, int *int_1)
{
    double a, b, c, d;
    int x1, y1;
    double lat_1, lon_1;
    int in;

    if (fabs(x_2 - x_1) > 1e-5) { /* Slope may be calculated */
        a = (y_2 - y_1)/(x_2 - x_1);
        b = y_1 - a * x_1;
        if ((fabs(x_4 - x_3) < 1e-5)) {     /* This slope is infinite */
          /* calculate intersection */
          *x = x_3;
          *y = a*x_3 + b;
          *int_1 = TRUE;
        } else {                            /* Both slopes may be calculated */
          c = (y_4 - y_3)/(x_4 - x_3);
          d = y_3 - c * x_3;
          if (fabs(a - c) < 1e-5) {         /* Slopes the same, no intersection */
            *int_1 = FALSE;
          } else {                          /* calculate intersection */
            *x = (d - b)/(a - c);
            *y = (a*d - b*c)/(a - c);
            *int_1 = TRUE;
          }
        }
    } else {                              /* Slope is infinite */
        if ((fabs(x_4 - x_3) < 1e-5)) {     /* this slope is also infinite */
          *int_1 = FALSE;
        } else {                            /* There's an intersection */
          c = (y_4 - y_3)/(x_4 - x_3);
          d = y_3 - c * x_3;
          *x = x_1;
          *y = c*x_1 + d;
          *int_1 = TRUE;
        }
    }

    if (*int_1) {
        if (((((y_1 - Fuz) <= *y) && (*y <= (y_2 + Fuz)))
             || (((y_2 - Fuz) <= *y) && (*y <= (y_1 + Fuz))))
            && ((((x_1 - Fuz) <= *x) && (*x <= (x_2 + Fuz)))
                || (((x_2 - Fuz) <= *x) && (*x <= (x_1 + Fuz)))))
              *int_1 = TRUE;
        else
              *int_1 = FALSE;
    }

    if (*int_1) {
        inv_gt(*x, *y, &lat_1, &lon_1);
        xform(lat_1, lon_1, &x1, &y1, &in);
        if (!in)
            *int_1 = FALSE;
    }
}

static void circ_intersect(double x_1, double y_1, double x_2, double y_2,
                           double r, double *x1, double *y1, int *int_1,
                                     double *x2, double *y2, int *int_2)
{
    double c, d;
    double xroot1, xroot2;
    double yr1 = 0, yr2 = 0, r1, r2;
    int n;
    int xt1, yt1;
    double lat_1, lon_1;
    int in;

    if (fabs(x_2 - x_1) < 1e-5) {         /* Line has infinite slope */
        xroot1 = xroot2 = *x1 = *x2 = x_1;
        if (fabs(r) > fabs(x_1)) {
          yr1 = sqrt(r * r - x_1 * x_1);
          yr2 = -yr1;
          n = 2;
          *int_1 = *int_2 = TRUE;
        } else if (fabs(r) == fabs(x_1)) {
          yr1 = 0;
          n = 1;
          *int_1 = *int_2 = TRUE;
        } else {
          n = 0;
          *int_1 = *int_2 = FALSE;
        }
        } else {                              /* Line slope may be calculated */
        c = (y_2 - y_1)/(x_2 - x_1);
        d = y_1 - c * x_1;
        quadrat(1 + c * c, 2 * c * d, d * d - r * r,
                &xroot1, &xroot2, &n);
        if (n == 0) {               /* No intersection */
          *int_1 = *int_2 = FALSE;
        } else if (n == 1) {        /* One intersection */
          *x1 = xroot1;
          *y1 = c * xroot1 + d;
          *int_1 = TRUE;
          *int_2 = FALSE;
        } else {                    /* Two intersections */
          yr1 = c*xroot1 + d;
          yr2 = c*xroot2 + d;
          *int_1 = *int_2 = TRUE;
        }
    }

    if (n == 2) {
        r1 = (xroot1 - x_1) * (xroot1 - x_1) + (yr1 - y_1) * (yr1 - y_1)
              + (xroot1 - x_2) * (xroot1 - x_2) + (yr1 - y_2) * (yr1 - y_2);
        r2 = (xroot2 - x_1) * (xroot2 - x_1) + (yr2 - y_1) * (yr2 - y_1)
              + (xroot2 - x_2) * (xroot2 - x_2) + (yr2 - y_2) * (yr2 - y_2);
        if (r1 > r2) {
          *x1 = xroot2;
          *y1 = yr2;
          *x2 = xroot1;
          *y2 = yr1;
          *int_1 = *int_2 = TRUE;
        } else {
          *x1 = xroot1;
          *y1 = yr1;
          *x2 = xroot2;
          *y2 = yr2;
          *int_1 = *int_2 = TRUE;
        }
    }

    if (*int_1) {
        if ((((y_1 <= *y1) && (*y1 <= y_2)) || ((y_2 <= *y1) && (*y1 <= y_1)))
            && (((x_1 <= *x1) && (*x1 <= x_2)) || ((x_2 <= *x1) && (*x1 <= x_1))))
          *int_1 = TRUE;
        else
          *int_1 = FALSE;
    }

        if (*int_1) {
        inv_gt(*x1, *y1, &lat_1, &lon_1);
        xform(lat_1, lon_1, &xt1, &yt1, &in);
        if (!in)
            *int_1 = FALSE;
    }

    if (*int_2) {
        if ((((y_1 <= *y2) && (*y2 <= y_2)) || ((y_2 <= *y2) && (*y2 <= y_1)))
            && (((x_1 <= *x2) && (*x2 <= x_2)) || ((x_2 <= *x2) && (*x2 <= x_1))))
          *int_2 = TRUE;
        else
          *int_2 = FALSE;
    }

        if (*int_2) {
            inv_gt(*x2, *y2, &lat_1, &lon_1);
            xform(lat_1, lon_1, &xt1, &yt1, &in);
            if (!in)
                *int_2 = FALSE;
        }

}


/* defines and clipped_at are for use in area drawing,
   to indicate that a corner has fallen in the area */
#define NO_CLIP 0
#define WEST_CLIP 1
#define EAST_CLIP 2
#define NORTH_CLIP 4
#define SOUTH_CLIP 8
#define RADIUS_CLIP 16
#define NE_CORNER 6
#define SE_CORNER 10
#define NW_CORNER 5
#define SW_CORNER 9

static int clip_at1, clip_at2;

/* return transformed values clipped so both endpoints are inregion */
/* return lats and lons */
/* The line is a great circle on the celestial sphere,
   or a line in lat and long (e.g. the line 0h 0d to 2h 10d
                              contains the point at 0.5h 5d).

   Gnomonic transformation maps a great circle to a line.

   There are three possibilities:
   1) both endpoints are in the region.
   2) one endpoint is in the region.
   3) both endpoints are outside of the drawn region.

In case 1, nothing needs to be done.
In case 2, a second point at the intersection of the line and the boundary of
the drawn region is calculated.
In case 3, it is possible that some segment of the line is in the drawn region,
and two points along the boundary are calculated.

The boundary of the drawn region, projected through gnomonic transformation,
are:
Sansons: lines on east and west, a line, a parabola, or an ellipse
                to the north and south.
                ellipse iff declination of boundary > (90 - decl. of center)
Stereographic: circle
Gnomonic: rectangle
Orthographic: rectangle in orthographic is very comples, use a circle.
Simple: same as sansons.
*/


int clipr_xform(double lat1, double lon1, double lat2, double lon2,
                int *xloc1, int *yloc1, int *xloc2, int *yloc2, int great_circle,
                double *plat1, double *plon1, double *plat2, double *plon2)
{
    int Lisin, Risin;
    double x_1, y_1, x_2, y_2;
    double theta_1, theta_2;
    int int_w, int_e, int_n, int_s, int_r1 = 0, int_r2 = 0;
    double xw, xe, xn, xs, xr1 = 0.0, xr2 = 0.0,
           yw, ye, yn, ys, yr1 = 0.0, yr2 = 0.0;

    double Llat, Llon, Rlat, Rlon, Mlat, Mlon;
    int Lx, Ly, Rx, Ry, Mx, My;
    int inL, inR, inM;


    *plon1 = lon1;
    *plon2 = lon2;
    *plat1 = lat1;
    *plat2 = lat2;
    clip_at1 = clip_at2 = NO_CLIP;
    xform(lat1, lon1, xloc1, yloc1, &Lisin);
    xform(lat2, lon2, xloc2, yloc2, &Risin);
    if (Lisin && Risin)           /* is already ok: case 1 */
        return TRUE;

    if (great_circle && gt_use_boundaries) {
        /* Transform to gnomonic */
        do_gt(lat1, lon1, &x_1, &y_1, &theta_1);
        do_gt(lat2, lon2, &x_2, &y_2, &theta_2);

        if ((theta_1 > 1.57) || (theta_2 > 1.57)) { /* out of field, skip */
          return FALSE;
        }

        /* Find intersections with boundaries */
        switch (xfs_proj_mode) {

            case GNOMONIC:
              line_intersect(x_1, y_1, x_2, y_2, gt_wx1, gt_wy1, gt_wx2, gt_wy2,
                             &xw, &yw, &int_w);
              line_intersect(x_1, y_1, x_2, y_2, gt_ex1, gt_ey1, gt_ex2, gt_ey2,
                             &xe, &ye, &int_e);
              line_intersect(x_1, y_1, x_2, y_2, gt_ex2, gt_ey2, gt_wx2, gt_wy2,
                             &xn, &yn, &int_n);
              line_intersect(x_1, y_1, x_2, y_2, gt_wx1, gt_wy1, gt_ex1, gt_ey1,
                             &xs, &ys, &int_s);
              int_r1 = int_r2 = FALSE;
              break;

            case STEREOGR:
              circ_intersect(x_1, y_1, x_2, y_2, gt_r, &xr1, &yr1, &int_r1,
                              &xr2, &yr2, &int_r2);
              int_w = int_n = int_s = int_e = FALSE;

            case ORTHOGR:
              break;

            default:                      /* error */
              break;
        }


        if (!(!Lisin && !Risin)) {             /* case 2 */
          if (int_w) {
            x_1 = xw; y_1 = yw;
            if (Risin)
              clip_at1 = WEST_CLIP;
            else
              clip_at2 = WEST_CLIP;
          } else if (int_e) {
            x_1 = xe; y_1 = ye;
            if (Risin)
              clip_at1 = EAST_CLIP;
            else
              clip_at2 = EAST_CLIP;
          } else if (int_n) {
            x_1 = xn; y_1 = yn;
            if (Risin)
              clip_at1 = NORTH_CLIP;
            else
              clip_at2 = NORTH_CLIP;
          } else if (int_s) {
            x_1 = xs; y_1 = ys;
            if (Risin)
              clip_at1 = SOUTH_CLIP;
            else
              clip_at2 = SOUTH_CLIP;
          } else if (int_r1) {
            x_1 = xr1; y_1 = yr1;
            if (Risin)
              clip_at1 = RADIUS_CLIP;
            else
              clip_at2 = RADIUS_CLIP;
          } else {
        /*      fprintf(stderr, "Error drawing vector\n");
            fprintf(stderr, "from (%.3f %.3f) to (%.3f %.3f)\n",
                    lat1, lon1, lat2, lon2);*/
            return FALSE;
          }
          if (Lisin) {                /* Need to find new point 2 */
            inv_gt(x_1, y_1, plat2, plon2);
            xform(*plat2, *plon2, xloc2, yloc2, &inM);
          } else {                            /* Need to find new point 1 */
            inv_gt(x_1, y_1, plat1, plon1);
            xform(*plat1, *plon1, xloc1, yloc1, &inM);
          }
        } else {                              /* case 3 */
          if (int_w && int_e) {
            x_1 = xw; y_1 = yw;
            x_2 = xe; y_2 = ye;
            clip_at1 = WEST_CLIP;
            clip_at2 = EAST_CLIP;
          } else if (int_w && int_n) {
            x_1 = xw; y_1 = yw;
            x_2 = xn; y_2 = yn;
            clip_at1 = WEST_CLIP;
            clip_at2 = NORTH_CLIP;
          } else if (int_w && int_s) {
            x_1 = xw; y_1 = yw;
            x_2 = xs; y_2 = ys;
            clip_at1 = WEST_CLIP;
            clip_at2 = SOUTH_CLIP;
          } else if (int_e && int_n) {
            x_1 = xe; y_1 = ye;
            x_2 = xn; y_2 = yn;
            clip_at1 = EAST_CLIP;
            clip_at2 = NORTH_CLIP;
          } else if (int_e && int_s) {
            x_1 = xe; y_1 = ye;
            x_2 = xs; y_2 = ys;
            clip_at1 = EAST_CLIP;
            clip_at2 = SOUTH_CLIP;
          } else if (int_n && int_s) {
            x_1 = xn; y_1 = yn;
            x_2 = xs; y_2 = ys;
            clip_at1 = NORTH_CLIP;
            clip_at2 = SOUTH_CLIP;
          } else if (int_r1 && int_r2) {
            x_1 = xr1; y_1 = yr1;
            x_2 = xr2; y_2 = yr2;
            clip_at1 = clip_at2 = RADIUS_CLIP;
          } else return FALSE;

          inv_gt(x_1, y_1, plat1, plon1);
          inv_gt(x_2, y_2, plat2, plon2);
          xform(*plat1, *plon1, xloc1, yloc1, &inM);
          xform(*plat2, *plon2, xloc2, yloc2, &inM);
        }
        return TRUE;
    } else {                            /* find boundaries by bisection */

        if (!Lisin && !Risin)    /* is hopeless */
          return FALSE;

        /* Now, one side is in, and the other out.  Make sure we won't have
           problems with crossing 0h */
        /* If the difference between lon1 and lon2 is greater than
           the difference if you subtract 360 from the larger,
           then shift the larger by 360 degrees */

        if (fabs(MAX(lon1, lon2) - MIN(lon1, lon2))
            > fabs(MAX(lon1, lon2) - 360.0 - MIN(lon1, lon2))) {
          if (lon2 > 180.0) {
            lon2 -= 360.0;
          } else {
            lon1 -= 360.0;
          }
        }

        Llat = lat1;
        Llon = lon1;
        Rlat = lat2;
        Rlon = lon2;
        xform(Llat, Llon, &Lx, &Ly, &inL);
        xform(Rlat, Rlon, &Rx, &Ry, &inR);


        /* One endpoint is in.
           Now use bisection to find point at edge */
        do {
          if (great_circle) {
            gcmidpoint(Llat, Llon, Rlat, Rlon, &Mlat, &Mlon);
          } else {
            Mlat = (Llat + Rlat) / 2.0;
            Mlon = (Llon + Rlon) / 2.0;
          }
          xform(Mlat, Mlon, &Mx, &My, &inM);

          if (inL)                       /* L in R out */
            if (inM) {           /* between M and R */
              Llat = Mlat;
              Llon = Mlon;
              inL = inM;
              Lx = Mx;
              Ly = My;
            } else {             /* between M and L */
              Rlat = Mlat;
              Rlon = Mlon;
              inR = inM;
              Rx = Mx;
              Ry = My;
            }
          else                   /* L out R in */
            if (inM) {           /* between M and L */
              Rlat = Mlat;
              Rlon = Mlon;
              inR = inM;
              Rx = Mx;
              Ry = My;
            } else {             /* between M and R */
              Llat = Mlat;
              Llon = Mlon;
              inL = inM;
              Lx = Mx;
              Ly = My;
            }
        } while ((fabs((Llat - Rlat)) > xf_c_scale) ||
                 (fabs((Llon - Rlon)) > xf_c_scale));

        if (Lisin) {             /* Left point is in,
                                        bisection found right point */
          *xloc2 = Lx;               /* Use Lx, Ly, since they're inside bounds */
          *yloc2 = Ly;
          *plon2 = Llon;
          *plat2 = Llat;
        } else {                         /* Bisection found left point */
          *xloc1 = Rx;               /* Use Rx, Ry, since they're inside bounds */
          *yloc1 = Ry;
          *plon1 = Rlon;
          *plat1 = Rlat;
        }

        return TRUE;
    }
}

/* Draw a curved line between points 1 and 2.
clipxform has been called, xloc1, yloc1, xloc2, yloc2 are in bounds */

#ifdef DRAWCURVELINE_DEBUG
long dcl0 = 0, dclt = 0;
#endif

void drawcurveline(double  lat1, double lon1, double lat2, double lon2,
                   int xloc1, int yloc1, int xloc2, int yloc2,
                   int line_style, int great_circle, int clevel)
{
  double mlat, mlon;    /* midpoint lat and long */
  int mxloc, myloc;     /* transformed */
  int mpx, mpy;         /* from given x,y */
  int inregion;

#ifdef DRAWCURVELINE_DEBUG
if (clevel == 0) {
    dcl0++;
}
if (clevel == 0 || dcl0 == 435) {
fprintf(stderr, "%ld: Drawcurveline(%.6f, %.6f, %.6f, %.6f, %d, %d, %d, %d, %d, %d, %d)\n",
    dcl0, lat1, lon1, lat2, lon2, xloc1, yloc1, xloc2, yloc2, line_style, great_circle, clevel);
}
dclt++;
#endif
/*
fprintf(stderr, "Drawcurveline(%.2f, %.2f, %.2f, %.2f, %d, %d, %d, %d, %d, %d, %d)\n",
    lat1, lon1, lat2, lon2, xloc1, yloc1, xloc2, yloc2, line_style, great_circle, clevel);
*/
/* ra difference should be less than 180 degrees: take shortest path */
  if ((xfs_proj_mode == STEREOGR) || (xfs_proj_mode == GNOMONIC)
      || (xfs_proj_mode == ORTHOGR)) {
        if ((lon1 - lon2) > 180.0) {
            lon1 -= 360.0;
        } else {
            if ((lon2 - lon1) > 180.0) {
                lon2 -= 360.0;
            }
        }
  }

  if (great_circle) {
    gcmidpoint(lat1, lon1, lat2, lon2, &mlat, &mlon);
  } else {
    mlat = (lat1 + lat2) / 2;
    mlon = (lon1 + lon2) / 2;
  }

  xform(mlat, mlon, &mxloc, &myloc, &inregion);
/*assert(inregion);*/

  mpx = (xloc1 + xloc2) / 2;
  mpy = (yloc1 + yloc2) / 2;

#define MAXRECURSE 100
  if (inregion && ((abs(mpx - mxloc) + abs(mpy - myloc)) > 0) && (clevel < MAXRECURSE)) {
#ifdef DRAWCURVELINE_DEBUG
if (dcl0 == 435) {
    fprintf(stderr, "  Mxloc %d  myloc %d  mlat %.6f mlon %.6f  mpx %d  mpy %d\n",
        mxloc, myloc, mlat, mlon, mpx, mpy);
}
#endif
    /* center is not where it should be */
    drawcurveline(lat1, lon1, mlat, mlon, xloc1, yloc1,
                  mxloc, myloc, line_style, great_circle, clevel + 1);
    drawcurveline(mlat, mlon, lat2, lon2, mxloc, myloc,
                  xloc2, yloc2, line_style, great_circle, clevel + 1);
  } else {
    drawVector(xloc1, yloc1, xloc2, yloc2);
/*  D_movedraw(xloc1, yloc1, xloc2, yloc2, line_style);  */
  }
}
