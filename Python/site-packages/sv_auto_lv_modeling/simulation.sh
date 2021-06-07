# Generate Simulation Ready File for svFSI

# Id of the phase to generate volume mesh
# This information can be obtained from the 
# last line of the shell output of run_volmesh.sh
phase_id=9
p_id=WS01
# Path to the registered surface meshes
input_dir=./04-SurfReg/$p_id
# Path to the outputed volume meshes
output_dir=./05-VolMesh/$p_id
# Number of interpolations between adjacent phases
num=99
# Number of cardiac cycles
cyc=1
# Cycle duriation in seconds
period=1.25

# Write boundary conditions for FSI simulations
python ./Modeling/svfsi/interpolation.py \
    --input_dir $input_dir \
    --output_dir $output_dir \
    --num_interpolation $num \
    --num_cycle $cyc \
    --duration $period \
    --phase $phase_id \
    --boundary_type displacement 

