open System
open System.Collections.Generic

let K = 1.3806485279E-23

let STEPS = 1000 // Resolution of the cumulative distribution function (number of bins)
let MINP = 5E-6 // Where to mark the end of the Boltzmann curve (how close to 0 on y-axis)
let NOFFSET = 1.01 // Percent that the particle count is increased by during velocity generation

let mutable velocityPerStep = 1.0 // This is the initial value and should increase (meters / step * second)

let promptNumber name =
    printfn "Please enter the %s:" name
    Double.Parse(Console.ReadLine())

[<EntryPoint>]
let main argv = 
    let N = promptNumber "number of particles"
    let M = promptNumber "mass of one particle (kg)" // Neon: 3.3509177E-26
    let T = promptNumber "tempertature of the system (K)" // Room Temperature: 300
    
    let p v = // Two dimensional Maxwell Boltzman probability density function p(v)
        let MvKT = (M * v)/(K * T)
        MvKT * (Math.E ** (-0.5 * v * MvKT))
    
    let maximumVelocity = // Scrub through p(v) looking for a point lower than MINP
        let mutable testVelocity = velocityPerStep
        while (p testVelocity) > MINP do
            testVelocity <- testVelocity + velocityPerStep
        testVelocity
    velocityPerStep <- maximumVelocity / (float)STEPS // Set real conversion value
    
    let mutable sum = 0.0; // This will be used to integrate p(v) for every value of v
    let cumulativeProbabilities = 
        [| for i in 1 .. STEPS ->
            sum <- (p (float i * velocityPerStep) * velocityPerStep) + sum
            sum // The above code adds another rectangle's area to the right Rieman sum
        |]
    
    let particleVelocities = // Generates a perfect distribution with N particles
        [| for i in 1 .. (int N) ->
            let percentile = float i / (float N * NOFFSET) // Calculate velocity percentile
            let weightedIndex = 
                try // Search for first velocity with a higher percentile than calculated
                    Array.findIndex (fun p -> (percentile < p)) cumulativeProbabilities
                with // For some samples a high-enough percentile can't be found--use the max
                    | :? KeyNotFoundException -> STEPS - 1
            velocityPerStep * float weightedIndex // Convert array index to velocity
        |]
    
    printfn "%A" particleVelocities
    
    0 // return an integer exit code
