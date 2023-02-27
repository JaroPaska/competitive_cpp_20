cmake_minimum_required(VERSION 3.14)

project(competitive_cpp)

add_executable(${BINARY_NAME} ${MAIN_FILE})
target_compile_features(${BINARY_NAME} PUBLIC cxx_std_20)
target_compile_definitions(
        ${BINARY_NAME} PUBLIC
        $<$<CONFIG:Debug>:LOGGING SYNC_IO>
        $<$<CONFIG:Release>:>
        $<$<CONFIG:RelWithDebInfo>:LOGGING>
        $<$<CONFIG:MinSizeRel>:>
)

install(
        TARGETS ${BINARY_NAME}
        DESTINATION ${BINARY_DIR}
)
