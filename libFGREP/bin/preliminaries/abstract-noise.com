# Abstract, noisy : one quarter of all classes below level 1 shares
#                   one feature or ability with a peer

# instances of class-000 -- 011 choose one feature at random from feature-14 -- 15

# class-000
has class-000 feature-0
can class-000 ability-0
has class-000 feature-2
can class-000 ability-2
has class-000 feature-6
can class-000 ability-9
has class-000 feature-15
has class-000 feature-19

# class-001
has class-001 feature-0
can class-001 ability-0
has class-001 feature-2
can class-001 ability-2
has class-001 feature-7
can class-001 ability-7
has class-000 feature-14
has class-000 feature-21

# class-010
has class-010 feature-0
can class-010 ability-0
has class-010 feature-3
can class-010 ability-3
has class-010 feature-8
can class-010 ability-8
has class-000 feature-14
has class-000 feature-20

# class-011
has class-011 feature-0
can class-011 ability-0
has class-011 feature-3
can class-011 ability-3
has class-011 feature-9
can class-011 ability-9
has class-000 feature-14
has class-000 feature-19

# instances of class-100 -- 111 choose one feature at random from feature-16 -- 17
# class-100
has class-100 feature-1
can class-100 ability-1
has class-100 feature-3
can class-100 ability-4
has class-100 feature-10
can class-100 ability-10
has class-000 feature-16
has class-000 feature-20

# class-101
has class-101 feature-1
can class-101 ability-1
has class-101 feature-3
can class-101 ability-4
has class-101 feature-11
can class-101 ability-11
has class-000 feature-16
has class-000 feature-18

# class-110
has class-110 feature-1
can class-110 ability-1
has class-110 feature-5
can class-110 ability-5
has class-110 feature-12
can class-110 ability-12
has class-000 feature-16
has class-000 feature-18

# class-111
has class-111 feature-1
can class-111 ability-1
has class-111 feature-5
can class-111 ability-5
has class-111 feature-11
can class-111 ability-13
has class-000 feature-17
has class-000 feature-20


# instances of class-000 -- 011 choose one feature at random from feature-14 -- 15
# instances of class-100 -- 111 choose one feature at random from feature-16 -- 17
# instances of class-000 -- 111 choose two features at random from feature-18 -- 21


# generate between 1 and 4 instances of classes 000 -- 111
