############
### PAD ####
############

# description: Function to obtaine PAD from a lidar point cloud preprocessed output (e.g fraytracing)
# Ni: Number of hit
# N : number of ray pathing through
# G : leaf projection factor (defaul is 0.5 for a spherical leaf angle distribution)
# Z : distance traveled in the strata : d/cos(theta). With d being the peth of the strata
# Cover: cover of the vegetation to take into account heterogenieti at the plot scale leve (default is one)
# omega: the clumping factor (default is one)

# Output :
# A vector with PAD estimates


fPAD=function(Ni,N,G=0.5,z=1,Cover=1,omega=1) {
  (-log(1-Ni/(N*Cover))/(G*omega*z))*Cover
}
