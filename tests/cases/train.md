# Preface
the information provided in this document is summarized from
1. original paper:  [opportunity](https://archive.ics.uci.edu/dataset/226/opportunity+activity+recognition)
2. the new paper: [opportunity++](https://ieee-dataport.org/open-access/opportunity-multimodal-dataset-video-and-wearable-object-and-ambient-sensors-based)
please consider both since the new one is **missing** some important details


# Abstract
Opportunity is dataset used for Human Activity Recognition (HAR) that contains data from _4 users_ performing **everyday living activities** in a kitchen environment. For each user the dataset contains multiple runs:

## Runs
a run is a full session in which data recorded. there are 2 variation for a run in this dataset:

### ADL run // (5 times)
The ADL run consists of _temporally unfolding situations_: 
1. **Start**: lying on the deckchair, get up
2. **Groom**: move in the room, check that all the objects are in the right places in the drawers and on shelves
3. **Relax**: go outside and have a walk around the building
4. **Prepare coffee**: prepare a coffee with milk and sugar using the coffee machine
5. **Drink coffee**: take coffee sips, move around in the environment
6. **Prepare sandwich**: include bread, cheese and salami, using the bread cutter and various knifes and plates
7. **Eat sandwich**
8. **Cleanup**: put objects used to original place or dish washer, cleanup the table 
9. **Break**: lie on the deckchair
The users were given _complete liberty_ in executing these activities in the most natural way for them. This makes these runs particularly challenging for the recognition of human activities.

In each situation (like preparing sandwich), a large number of action primitives occur:
- reach for bread
- move to bread cutter
- operate bread cutter
- …

### Drill run // (1 time)
The drill run consists of 20 repetitions of the following sequence of activities:
1. Open then close the fridge
2. Open then close the dishwasher
3. Open then close 3 drawers (at different heights) 
4. Open then close door 1
5. Open then close door 2
6. Toggle the lights on then off
7. Clean the table
8. Drink while standing
9. Drink while seated

designed to generate a large number of activity instances in a more constrained scenario
 
 ----------

# Folder Structure  
* `{person}-{run}/`
	- `{user}-{run}_side.avi` : new, anonymized video of the person (10 fps)
	- `{user}-{run}_pose.csv` : new, created by OpenPose // (skeleton tracking data)
	- `{user}-{run}_sensors_data.txt` : old
	- `{user}-{run}_{label}.srt`: extracted labels from `{user}-{run}_sensors_data.txt`, exported as video subtitle


# Columns of `sensors_data.txt`
in the file `column_names.txt`, the order of columns are as follows

###  
## 1. time-track
first column is `MILLISEC` and measured time in milli-seconds (ms)

## Body-worn sensors

### Naming
sensors
- BACK: Back
- RUA: Right Upper Arm
- RLA: Right Lower Arm
- LUA: Left Upper Arm
- LLA: Left Lower Arm
- RH: Right Hand
- RWR: Right Wrist 
- HIP: Hip
- RKN: Right Knee
- R-Shoe: Right Shoe
- L-Shoe: Left Shoe

the notation `^` and `_` means upper and lower but in local neighborhood


### 2-37 some accelerometers
![[accelerator-opportunity-dataset.png| 400]]
// position of accelerator sensors on person's body

recorded data is only Acceleration in 3D // `acc[X|Y|Z]`
 
### 38-102 some [[IMU]]s
![[imu-sensors-opportunity-dataset.png| 400]]
// position of IMU sensors on person's body

[[IMU]] sensors measure the following data
- acceleration in X,Y,Z as `acc[X|Y|Z]`
- gyro as `gyro[X|Y|Z]`
- magnetic as `magnetic[X|Y|Z]`
- quaternion as `Quaternion[1|2|3|4]`


### 103-134 shoes
![[sensors-on-shoes-opportunity-dataset.png| 700]]
// sensors attached to the shoes

some sensors are attached to  `L-SHOE` and `R-SHOE` or simply left and right shoe and they provide following data:

* `Eu[X|Y|Z]`: Euler degrees: the X, Y, Z corresponds to the a _Roll_, _Pitch_, _Yaw_ Axis in the [[Euler orientation]].
* `Nav_A[x|y|z]`: Position relative to room's navigation system // (enter of the room)
* `Body_A[x|y|z]`: Position relative to the the person's **body** (sensors that are attached to the body), Axes are fixed to the object: x forward, y right, z down (common in aerospace)
* `AngVelBodyFrame[X|Y|Z]`: Angular rotation speed with respect to body's frame // i.e. How fast it's rotating around its own x, y, z axes (Typically from gyroscopes)
* `AngVelNavFrame[X|Y|Z]`: Angular rotation speed in world coordinate system. // i.e. How fast its orientation is changing in the world
* `Compass`: Heading/direction relative to magnetic north in degrees // (like in a real compass)


## 135-194 Object sensors 
![[object-sensors-opportunity-dataset.png| 500]]
// example of sensors that are attached to the objects

sensors attached to the objects
1. CUP 
2. SALAMI 
3. WATER
4. CHEESE
5. BREAD
6. KNIFE1
7. MILK
8. SPOON
9. SUGAR
10. KNIFE2 
11. PLATE
12. GLASS
and provide acceleration and gyro as 
- `acc[X|Y|Z]`
- `gyro[X|Y]`

## Ambient sensors

### 195-207 [[REED Switch]]es
[[REED switch]]es are magnetic switches that are enabled when there is magnetic field around. there are few of these in the environment:
1. DISHWASHER: S1 … S2
2. FRIDGE: S1 … S3
3. MIDDLEDRAWER: S1 … S3
4. LOWERDRAWER: S1 … S3
5. UPPERDRAWER

![[reed-switch-placements--opportunity-dataset.png| 600]]
// Reed switches usage example

with help of them, you can detect states like
- closed
- half open
- fully open
 

### 208-231 some more Accelerometers

![[wide-view-room-opportunity-dataset.png| 800]]
// map of room in which dataset is recorded

the acceleration of these objects are recorded:
1. DOOR1
2. LAZYCHAIR
3. DOOR2
4. DISHWASHER
5. UPPERDRAWER
6. LOWERDRAWER
7. MIDDLEDRAWER
8. FRIDGE

### 232-243 location tags
 
## 244-250 Label columns

these columns contain labels named:
1. `Locomotion`
2. `HL_Activity`
3. `LL_Left_Arm`
4. `LL_Left_Arm_Object`
5. `LL_Right_Arm`
6. `LL_Right_Arm_Object`
7. `ML_Both_Arms`


the labels focused on the _multimodal_ perception and learning of human activities that are categorized in **multi-level** fashion:
- short actions
- gestures
- modes of locomotion
- higher-level behavior
![[multi-level-labeling-opportunity-dataset.png| 700]]
// example of multimodal perception

all possible values for label is listed in the file `label_legend.txt`.

----

# Columns of `pose.csv`

* `Frame_Number` identify the frame number
* `Person_number` identify a single person in the frame. Multiple people likely appear in each frame, as the experiment leaders were in the room. The data of all the people which OpenPose could detect are provided. However, there is **no guarantee** that a given person index refers to the same individual frame after frame, as no temporal tracking is implemented. This must be implemented by the researchers, for example by looking at continuity from frame to frame.
* `Participant` identify whether the person detected is the participant performing the data collection (value = 1), or a bystander (value = 0)
* `{body_part}_{x, y, confidence}`: for each body part, the x-y coordinates are provided, together with a confidence of the position estimation in the interval [0-1]. The possible {`body_part`} are 25 in total. Not all the body part are necessarily detected in frame for every person. In case a body part is not detected, the coordinates are 0,0. The body part are: `"Nose", "Neck", "RShoulder", "RElbow", "RWrist", "LShoulder", "LElbow", "LWrist", "MidHip", "RHip", "RKnee", "RAnkle", "LHip", "LKnee", "LAnkle", "REye", "LEye",  "REar", "LEar", "LBigToe", "LSmallToe", "LHeel", "RBigToe", "RSmallToe", "RHeel", "Background"`


# Comparison
here are some comparisons made by authors of other datasets:

## [OctoNet](https://openreview.net/forum?id=z3TftXOizf)
![[OctoNet-dataset-comparison-har.png| 1000]]
// comparison of HAR datasets by OctoNet dataset paper

## [CAPTURE-24](https://www.nature.com/articles/s41597-024-03960-3) 
![[capture-24-dataset-comparison-har.png| 1000]]
// comparison of HAR datasets by Capture-24 dataset paper