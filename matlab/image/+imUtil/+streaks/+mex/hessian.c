/*----------------------------------------------------------------------------*/
/*--------------------------------- Hessian ----------------------------------*/
/*----------------------------------------------------------------------------*/
/*     (substitutes Gradient, lines 731 -887 of the original code             */
/*----------------------------------------------------------------------------*/
/** Computes the direction of the level line of 'in' at each point.

    The result is:
    - an image_float with the angle at each pixel, or NOTDEF if not defined.
    - the image_float 'modgrad' (a pointer is passed as argument)
      with the gradient magnitude at each point.
    - a list of pixels 'list_p' roughly ordered by decreasing
      gradient magnitude. (The order is made by classifying points
      into bins by gradient magnitude. The parameters 'n_bins' and
      'max_grad' specify the number of bins and the gradient modulus
      at the highest bin. The pixels in the list would be in
      decreasing gradient magnitude, up to a precision of the size of
      the bins.)
    - a pointer 'mem_p' to the memory used by 'list_p' to be able to
      free the memory when it is not used anymore.
 */
static image_float ll_angle( image_float in, float threshold,
                              struct coorlist ** list_p, void ** mem_p,
                              image_float * modgrad, unsigned int n_bins )
{
  image_float g;
  unsigned int n,p,x,y,adrm,adr,adrp,i,yp;
  float A,B,C,D,E,F,G,H,I,E2,Et,Hxx,Hxy,Hyy,Discr,lambda1,lambda2,gx,gy,norm;
  /* the rest of the variables are used for pseudo-ordering
     the gradient magnitude values */
  int list_count = 0;
  struct coorlist * list;
  struct coorlist ** range_l_s; /* array of pointers to start of bin list */
  struct coorlist ** range_l_e; /* array of pointers to end of bin list */
  struct coorlist * start;
  struct coorlist * end;
  float max_grad = 0.0;

  /* check parameters */
  if( in == NULL || in->data == NULL || in->xsize == 0 || in->ysize == 0 )
    error("ll_angle: invalid image.");
  if( threshold < 0.0 ) error("ll_angle: 'threshold' must be positive.");
  if( list_p == NULL ) error("ll_angle: NULL pointer 'list_p'.");
  if( mem_p == NULL ) error("ll_angle: NULL pointer 'mem_p'.");
  if( modgrad == NULL ) error("ll_angle: NULL pointer 'modgrad'.");
  if( n_bins == 0 ) error("ll_angle: 'n_bins' must be positive.");

  /* image size shortcuts */
  n = in->ysize;
  p = in->xsize;

  /* allocate output image */
  g = new_image_float(in->xsize,in->ysize);

  /* get memory for the image of gradient modulus */
  *modgrad = new_image_float(in->xsize,in->ysize);

  /* get memory for "ordered" list of pixels */
  list = (struct coorlist *) calloc( (size_t) (n*p), sizeof(struct coorlist) );
  *mem_p = (void *) list;
  range_l_s = (struct coorlist **) calloc( (size_t) n_bins,
                                           sizeof(struct coorlist *) );
  range_l_e = (struct coorlist **) calloc( (size_t) n_bins,
                                           sizeof(struct coorlist *) );
  if( list == NULL || range_l_s == NULL || range_l_e == NULL )
    error("not enough memory.");
  for(i=0;i<n_bins;i++) range_l_s[i] = range_l_e[i] = NULL;

  /* 'undefined' on the four edges */
  for(x=0;x<p;x++) g->data[x]         = NOTDEF; // top
  for(y=0;y<n;y++) g->data[p*y]       = NOTDEF; // left
  for(x=0;x<p;x++) g->data[(n-1)*p+x] = NOTDEF; // down
  for(y=0;y<n;y++) g->data[p*y+p-1]   = NOTDEF; // right

  /* compute the hessian on the remaining pixels */
  for(y=1;y<n-1;y++)
    {
    yp = y*p;
    adr = yp+1;
    adrm = adr-p;
    adrp = adr+p;
    B = in->data[adrm];
    C = in->data[adrm+1];
    E = in->data[adr];
    F = in->data[adr+1];
    H = in->data[adrp];
    I = in->data[adrp+1];
    for(x=1;x<p-1;x++)
      {
        adr = yp+x;
        adrm = adr-p;
        adrp = adr+p;

        /*
           Hessian computation using 3x3 pixel window:
             A B C
             D E F
             G H I
           and
             Hxx= D -2*E + F
             Hyy= B -2*E + H
             Hxy= A - G - C + I 
           Then
             lambda2 = (Hxx + Hyy -sqrt((Hxx-Hyy)^2 +4 Hxy^2))/2
             g= atan2(Hxy,lambda2-Hxx)
           the best magnitude indicator is probably then
             norm = log(-lambda2) if lambda2<-threshold, 0 otherwise
         */

// some of these values could be carried out from one iteration
//  to the next, without seeking the whole array
/*
        A = in->data[adrm-1];
        B = in->data[adrm];
        C = in->data[adrm+1];
        D = in->data[adr-1];
        E = in->data[adr];
        F = in->data[adr+1];
        G = in->data[adrp-1];
        H = in->data[adrp];
        I = in->data[adrp+1];
*/
        A=B; B=C; C = in->data[adrm+1];
        D=E; E=F; F = in->data[adr+1];
        G=H; H=I; I = in->data[adrp+1];

        
        E2 = 2*E;
        Hxx= D -E2 + F;
        Hyy= B -E2 + H;
        Hxy= (A - G - C + I)/4;
        Discr = sqrt((Hxx-Hyy)*(Hxx-Hyy) +4*Hxy*Hxy);

        lambda1 = (Hxx + Hyy +Discr)/2;
        lambda2 = (Hxx + Hyy -Discr)/2;
        gx = Hxy; /* gradient x component */
        gy = lambda2-Hxx; /* gradient y component */

        //if (E>1) Et=E; else Et=1;
        Et=7.5;
        if( lambda2 < 0. && Et*lambda1<-lambda2) // FIXME empiric threshold
//            norm = sqrt(-lambda2); /* "gradient norm", compresses dynamics but takes time */
            norm = -lambda2; /* "gradient norm" */
        else
            norm = 0;

        (*modgrad)->data[adr] = norm; /* store gradient norm */

//        if( norm <= threshold ) /* norm too small, gradient no defined */
        if( norm <= threshold/100 ) /* FIXME, empirical */
          g->data[adr] = NOTDEF; /* gradient angle not defined */
        else
          {
            /* gradient angle computation */
            g->data[adr] = atan2(gx,-gy);

            /* look for the maximum of the gradient */
            if( norm > max_grad ) max_grad = norm;
          }
      }
  }

  /* compute histogram of gradient values */
  for(x=0;x<p-1;x++)
    for(y=0;y<n-1;y++)
      {
        norm = (*modgrad)->data[y*p+x];

        /* store the point in the right bin according to its norm */
        i = (unsigned int) (norm * (double) n_bins / max_grad);
        if( i >= n_bins ) i = n_bins-1;
        if( range_l_e[i] == NULL )
          range_l_s[i] = range_l_e[i] = list+list_count++;
        else
          {
            range_l_e[i]->next = list+list_count;
            range_l_e[i] = list+list_count++;
          }
        range_l_e[i]->x = (int) x;
        range_l_e[i]->y = (int) y;
        range_l_e[i]->next = NULL;
      }

  /* Make the list of pixels (almost) ordered by norm value.
     It starts by the larger bin, so the list starts by the
     pixels with the highest gradient value. Pixels would be ordered
     by norm value, up to a precision given by max_grad/n_bins.
   */
  for(i=n_bins-1; i>0 && range_l_s[i]==NULL; i--);
  start = range_l_s[i];
  end = range_l_e[i];
  if( start != NULL )
    while(i>0)
      {
        --i;
        if( range_l_s[i] != NULL )
          {
            end->next = range_l_s[i];
            end = range_l_e[i];
          }
      }
  *list_p = start;

  /* free memory */
  free( (void *) range_l_s );
  free( (void *) range_l_e );

  return g;
}

