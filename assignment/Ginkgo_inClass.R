data.frame("ginkgo_data_2021")

plot(ginkgo_data_2021$max_depth, ginkgo_data_2021$max_width,
     main = "Ginkgo Max Leaf Depth verses Width",
     xlab = "Max Depth (mm)",
     ylab = "Max Width (mm)")

boxplot(notch_depth ~ seeds_present, data = ginkgo_data_2021,
         main = "Boxplot Notch Depth and Seeds Presence",
         xlab = "Seeds Present",
         ylab = "Notch Depth")
