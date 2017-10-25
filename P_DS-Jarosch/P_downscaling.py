"""
Precipitation downscaling using a linear model

Basing on:
Jarosch et al., 2012. High-resolution precipitation and temperature downscaling for glacier models. Clim Dyn 38, 391–409
Smith, 2003. Advection, Diffusion, and Deposition From Distributed Sources. Boundary-Layer Meteorology 107, 273–287
Smith and Barstad, 2004. A Linear Theory of Orographic Precipitation. J. Atmos. Sci. 61, 1377–1391
Smith et al., 2005. Orographic Precipitation and Oregon’s Climate Transition. J. Atmos. Sci. 62


"""

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid.axislines import SubplotZero
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
from matplotlib.ticker import LinearLocator, FormatStrFormatter


def plot_topography(x, y, z, zlim_min, zlim_max):
    # Plot the surface.
    fig = plt.figure()
    ax = fig.gca(projection='3d')
    surf = ax.plot_surface(x, y, z, cmap=cm.coolwarm,
                           linewidth=0, antialiased=False)
    ax.set_zlim(zlim_min, zlim_max)
    ax.zaxis.set_major_locator(LinearLocator(11))
    ax.zaxis.set_major_formatter(FormatStrFormatter('%.02f'))
    fig.colorbar(surf, shrink=0.5, aspect=5)

    plt.show()

''' Variable definitions '''
# Input metereological data
# Todo: should mostly be imported from Reanalysis data
surface_vapor_density = 1       # [kg m-3]
moist_lapse_rate = 0.1          #
environmental_lapse_rate = 0.2  # ?


# Create demo topography
# Todo: replace with imported DEM

# Option 1: random topography
x = np.arange(1, 10, 1)
y = np.arange(1, 10, 1)
x, y = np.meshgrid(x, y)
base = np.random.rand(10, 10)
surface_elevation = x + y * base[x, y] * 100

# Option 2: Central hill
# x = np.arange(-5, 5, 0.25)
# y = np.arange(-5, 5, 0.25)
# x, y = np.meshgrid(x, y)
# R = np.sqrt(x**2 + y**2)
# surface_elevation = (np.sin(R) * 500) + 500


# Wind vector (northward, eastward) [m s-1]
# Typical values range from -30 to 30
wind_vector = np.array([-15, 12])

# surface_elevation = base * 500 + 1000
print("Surface elevation matrix: %s" % surface_elevation)
print("Wind vector: %s" % wind_vector)


''' Linear orographic precipitation downscaling model '''

# ĥ(k,l) Fourier transform of topography
# h: 2D Fourier transform of surface elevation h
# k,l: wavenumbers associated with space coordinates x and y
topography_fourier = np.fft.fft2(surface_elevation)
print("Fourier topography: %s" % topography_fourier)

wind_vector_fourier = np.fft.fft2(wind_vector)

# Uplift sensitivity factor (Cw)
uplift_sensitivity_factor = surface_vapor_density * moist_lapse_rate / environmental_lapse_rate

# Vertical wave number (m)
vertical_wave_nr = effective_moist_static_stability

# fourier_orographic_P = (uplift_sensitivity_factor * wind_vector_fourier * topography_fourier) / \
#                       ((1 - vertival_wave_nr * vapor_scale_height) * \
#                        (1 + wind_vector_fourier * condensation_timescale) * \
#                        (1 + wind_vector_fourier * fallout_timescale))

plot_topography(x, y, surface_elevation, 0, 1000)

