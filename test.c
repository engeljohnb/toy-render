ComputeLighting(P, N, V, s) 
{
    i = 0.0
    for light in scene.Lights 
    {
        if light.type == ambient 
	{
            i += light.intensity
        }
        else 
	{
            if light.type == point 
	    {
                L = light.position - P
            } 
	    else 
	    {
                L = light.direction
            }

            // Diffuse
            n_dot_l = dot(N, L)
            if n_dot_l > 0 
	    {
                i += light.intensity * n_dot_l/(length(N) * length(L))
            }

            // Specular
          if s != -1 
	  {
                R = 2 * N * dot(N, L) - L
                r_dot_v = dot(R, V)
              if r_dot_v > 0 
	      {
                    i += light.intensity * pow(r_dot_v/(length(R) * length(V)), s)
              }
          }
        }
    }
    return i
}
