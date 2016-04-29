data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type where
  Bicycle : Vehicle Pedal
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  Unicycle : Vehicle Pedal
  Motorcycle : (fuel: Nat) -> Vehicle Petrol
  Tram : Vehicle Electric
  ElectricCar : Vehicle Electric

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels Unicycle = 1
wheels (Motorcycle fuel) = 2
wheels (ElectricCar) = 4
wheels Tram = 20

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel Bicycle impossible
refuel Unicycle impossible
refuel (Motorcycle fuel) = Motorcycle 50

bmw : Vehicle Petrol
bmw = Car 20

cube : Vehicle Pedal
cube = Bicycle

let x = refuel bmw
let y = refuel cube
