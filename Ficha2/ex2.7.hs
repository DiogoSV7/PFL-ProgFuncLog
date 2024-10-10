-- a)
approx::Int -> Double
approx 0 = 4
approx n = 4 * (((-1) ^ n) / fromIntegral (2 * n + 1) + (approx (n - 1) / 4))

-- b)
approx'::Int -> Double
approx' 0 = 4
approx' n = 12 * (((-1) ^ n) / ((fromIntegral (n + 1))^2) + (approx' (n - 1)/12))