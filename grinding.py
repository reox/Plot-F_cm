#!/usr/bin/env python3
import math

# Calculate power requirement for external/internal cylindrical grinding
def cylindrical_grinding(v_c, v_w, k_c11, z, d, a_e, D, b, ny, grit, external=True):
    # Setting external to False result in internal grinding.
    # Beware: Then d must be larger than D!
    if not external and d < D:
        raise ValueError("internal grinding with larger tool not possible")

    # Factor between wheel speed and workpiece speed
    # v_c and v_w should match to each other!
    # actually you would look this one up in a table.
    # (See also table 12.20)
    q = v_c / v_w  # 1

    # From table 12.3 (for 150 grid and a_e 0.003):
    # effective grain distance
    #lambda_ke = 64.0
    # A Linear model from the data gives us ruffly this function:
    # This function is a little bit off, but gives reasonable results.
    # especially in the middle grit sizes it does not work well...
    # If you do not trust it, look it up in the book!
    #
    # The problem with this formular is, that the grit is in the unit "mesh"
    # and we need millimeter. As there is no formular to calculate the real grain
    # size from mesh, we need those tables.
    def lambda_ke(grit, a_e):
        return math.floor(25 + 0.28 * grit - 900.0 * a_e)

    print("Effective grain distance: {} mm".format(lambda_ke(grit, a_e)))

    # From table 12.4 (150 grid and h_m > 0.004):
    # Correction factor
    # K = 2.3
    # Again, we created a linear model for this value.
    # And again, it is far from perfect, but should be sufficiant for our purposes.
    # One problem is, we get h_m values larger than 0.004, and do not know
    # how the table is continued...
    def K(grit, h_m):
        return 2.868 + 96.0 * (1 / grit) - 333.333 * h_m

    # For both formulars, there should be more information in the work of "Salje",
    # but we can not find that one... it is not cited in the book :(


    def rotationspeed(D, v_c):
        return (v_c * 60 * 10**3) / (D * math.pi)

    n_c = rotationspeed(D, v_c)
    n_w = rotationspeed(d, v_w)

    print("Rotation Speed: Grinding Wheel {} 1/min / Workpiece {} 1/min".format(n_c, n_w))

    # middle chip thickness
    if external:
        h_m = (lambda_ke(grit, a_e) / q) * math.sqrt(a_e * (1/D + 1/d))
    else:
        h_m = (lambda_ke(grit, a_e) / q) * math.sqrt(a_e * (1/D - 1/d))
    print("Mittenspandicke: {} mm".format(h_m))

    # Specific cutting force
    k_c = 1**z/h_m**z * k_c11 * K(grit, h_m)
    print("Correction Factor: {}".format(K(grit, h_m)))

    # Average force per blade
    F_cm = b * h_m * k_c

    # Angle
    if external:
        phi = (360 / math.pi) * math.sqrt(a_e / (D * (1 + D/d)))
    else:
        phi = (360 / math.pi) * math.sqrt(a_e / (D * (1 - D/d)))

    # Number of teeth in work
    z_E = (D * math.pi * phi) / (lambda_ke(grit, a_e) * 360)

    # resulting force
    F_m = F_cm * z_E

    # Power required:
    P = (F_m * v_c) / ny

    print("Power Required: {} W".format(P))


# There are some values you need to look up in tables.
# These tables can be found in the book "Praxis der Zerspanungstechnik" by Jochen Dietrich.
#
# We some some values here as an example, you might want to adjust them.
#
# Material Parameters: hardened steel, for example CK45
# From table 12.20 (for hardened steel):
v_c = 35.0  # m/s
v_w = 0.17  # m/s
# From table 2.1 (for CK45):
k_c11 = 2220.0  # N/mm^2
z = 0.14  # 1
# Diameter of the part
d = 6.0  # mm

# Process parameter:
# For finishing passes
a_e = 0.003  # mm

# Diameter of wheel
D = 50.0  # mm
# working width
b = 13.0  # mm

# efficency of motor, let's say it is only 50%
ny = 0.5

# grinding wheel parameters: 150 grit
grit = 150


cylindrical_grinding(v_c, v_w, k_c11, z, d, a_e, D, b, ny, grit)


