// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "Lyte",
    platforms: [.macOS(.v13), .iOS(.v16)],
    products: [
        // .dynamic so we don't run out of exception personality functions
        .library(name: "Lyte", type: .dynamic, targets: ["Lyte"]),
    ],
    targets: [
        .binaryTarget(
            name: "CLyte",
            url: "https://github.com/audulus/lyte/releases/download/0.2/CLyte.xcframework.zip",
            checksum: "0bb87b31eb8c6feff3dfa78fd5a9fa8556627650d8aeaf776d4a7d43cff93353"
        ),
        .target(
            name: "Lyte",
            dependencies: ["CLyte"]
        ),
        .testTarget(
            name: "LyteTests",
            dependencies: ["Lyte"]
        ),
    ]
)
