# A-Simple-3DOF-Robotic-User-Controlled-Arm



Simple Arm project that helps in industrial fields, which is built using three hobby servo motors. Each has its own range of angles to rotate within. The desired angle of these motors depends on the PWM Signals. LCD used to show the angles of these motors. In Robotic Arm project written in PIC16f877a Assembly Language, and for hardware implimantaton we used Proteus simulation.


System Description

• three motors to control the Robotic Arm which has three angles’ ranges depending on each motor specification.

• The three Analoge inputs that entered to the PIC are the ones which are going to control the pulse width that entered each motor. The larger Analoge input we enter;    the larger pulse width will be generated.

• the PWM signals to specify the angle of each motor, and we generated these PWM signals using delays instead of using Timers, which makes the design more simple with no   need to deal with Interrupts.

• We have a 20ms time period, which must have a pulse that controls the motor’s angle, since the minimum pulse width that is entered, will move the motor’s angle to its
  minimum extreme value, and so for the maximum pulse, it moves the motor’s angle to its maximum extreme value. As depicted in Figure 1, the minimum Control pulse is    1ms, and the maximum Control pulse is 3.0279ms.

![image](https://user-images.githubusercontent.com/97694540/161565544-172ec9e0-b60e-45a8-ae9f-3cb8fdb32002.png)
